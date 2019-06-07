package com.wavesplatform

import java.io.File
import java.nio.ByteBuffer
import java.util

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.db.openDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{AddressId, Height, TxNum}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Transaction, TransactionParsers}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

//noinspection ScalaStyle
object Explorer extends ScorexLogging {
  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  private val keys = Array(
    "version",
    "height",
    "score",
    "block-at-height", // not used now
    "height-of",
    "waves-balance-history",
    "waves-balance",
    "assets-for-address",
    "asset-balance-history",
    "asset-balance",
    "asset-info-history",
    "asset-info",
    "lease-balance-history",
    "lease-balance",
    "lease-status-history",
    "lease-status",
    "filled-volume-and-fee-history",
    "filled-volume-and-fee",
    "transaction-info", // not used now
    "address-transaction-history",
    "address-transaction-ids-at-height",
    "changed-addresses",
    "transaction-ids-at-height", // not used now
    "address-id-of-alias",
    "last-address-id",
    "address-to-id",
    "id-of-address",
    "address-script-history",
    "address-script",
    "approved-features",
    "activated-features",
    "data-key-chunk-count",
    "data-key-chunk",
    "data-history",
    "data",
    "sponsorship-history",
    "sponsorship",
    "addresses-for-waves-seq-nr",
    "addresses-for-waves",
    "addresses-for-asset-seq-nr",
    "addresses-for-asset",
    "address-transaction-ids-seq-nr", // not used now
    "address-transaction-ids", // not used now
    "alias-is-disabled",
    "carry-fee-history",
    "carry-fee",
    "asset-script-history",
    "asset-script",
    "safe-rollback-height",
    "changed-data-keys",
    "block-header-at-height",
    "nth-transaction-info-at-height",
    "address-transaction-seq-nr",
    "address-transaction-height-type-and-nums",
    "transaction-height-and-nums-by-id",
    "block-transactions-fee",
    "invoke-script-result"
  )

  def main(args: Array[String]): Unit = {
    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")

    val settings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    log.info(s"Data directory: ${settings.dbSettings.directory}")

    val portfolioChanges = Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr)
    val db               = openDB(settings.dbSettings.directory)
    val reader           = new LevelDBWriter(db, portfolioChanges, settings.blockchainSettings.functionalitySettings, settings.dbSettings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {

      val flag = args(1).toUpperCase

      flag match {
        case "B" =>
          val maybeBlockId = Base58.tryDecodeWithLimit(args(2)).toOption.map(ByteStr.apply)
          if (maybeBlockId.isDefined) {
            val kBlockHeight     = Keys.heightOf(maybeBlockId.get)
            val blockHeightBytes = db.get(kBlockHeight.keyBytes)
            val maybeBlockHeight = kBlockHeight.parse(blockHeightBytes)
            maybeBlockHeight.foreach { h =>
              val kBlock     = Keys.blockHeaderBytesAt(Height(h))
              val blockBytes = db.get(kBlock.keyBytes)
              log.info(s"BlockId=${maybeBlockId.get} at h=$h: ${Base64.encode(blockBytes)}")
            }
          } else log.error("No block ID was provided")

        case "O" =>
          val orderId = Base58.tryDecodeWithLimit(args(2)).toOption.map(ByteStr.apply)
          if (orderId.isDefined) {
            val kVolumeAndFee = Keys.filledVolumeAndFee(orderId.get)(blockchainHeight)
            val bytes1        = db.get(kVolumeAndFee.keyBytes)
            val v             = kVolumeAndFee.parse(bytes1)
            log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: Volume = ${v.volume}, Fee = ${v.fee}")

            db.iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.FilledVolumeAndFeePrefix), orderId.get.arr))
              .filter { e =>
                def parseBytesHeight(bs: Array[Byte]): (Short, Array[Byte], Height) = {
                  val prefix = Shorts.fromByteArray(bs.take(2))
                  val height = Height(Ints.fromByteArray(bs.takeRight(4)))
                  val aux = bs.drop(2).dropRight(4)
                  (prefix, aux, height)
                }

                val (_, bs, _) = parseBytesHeight(e.getKey)
                ByteStr(bs) == orderId.get
              }
              .map(e => (Ints.fromByteArray(e.getKey.takeRight(4)), com.wavesplatform.database.readVolumeAndFee(e.getValue)))
              .foreach { case (h, v) => log.info(s"\t h = $h: Volume = ${v.volume}, Fee = ${v.fee}") }
          } else log.error("No order ID was provided")

        case "A" =>
          val address   = Address.fromString(args(2)).explicitGet()
          val aid       = Keys.addressId(address)
          val addressId = aid.parse(db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")

          db.iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.WavesBalancePrefix), AddressId.toBytes(addressId)))
            .map(e => (Ints.fromByteArray(e.getKey.takeRight(4)), Longs.fromByteArray(e.getValue)))
            .foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "AC" =>
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          log.info(s"Last address id: $lastAddressId")

        case "AD" =>
          val result        = new util.HashMap[Address, java.lang.Integer]()
          val lastAddressId = Keys.lastAddressId.parse(db.get(Keys.lastAddressId.keyBytes))
          for (id <- 1L to lastAddressId.getOrElse(0L)) {
            val k       = Keys.idToAddress(id)
            val address = k.parse(db.get(k.keyBytes))
            result.compute(address,
                           (_, prev) =>
                             prev match {
                               case null    => 1
                               case notNull => 1 + notNull
                           })
          }

          for ((k, v) <- result.asScala if v > 1) {
            log.info(s"$k,$v")
          }

        case "AA" =>
          val secondaryId = args(3)

          val address   = Address.fromString(args(2)).explicitGet()
          val asset     = IssuedAsset(ByteStr.decodeBase58(secondaryId).get)
          val ai        = Keys.addressId(address)
          val addressId = ai.parse(db.get(ai.keyBytes)).get
          log.info(s"Address ID = $addressId")

          db.iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.AssetBalancePrefix), AddressId.toBytes(addressId)))
            .filter { e =>
              val (_, _, assetId, _) = Keys.parseAddressBytesHeight(e.getKey)
              ByteStr(assetId) == asset.id
            }
            .map(e => (Ints.fromByteArray(e.getKey.takeRight(4)), Longs.fromByteArray(e.getValue)))
            .foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "S" =>
          log.info("Collecting DB stats")
          val iterator = db.iterator()
          val result   = new util.HashMap[Short, Stats]
          iterator.seekToFirst()
          while (iterator.hasNext) {
            val entry     = iterator.next()
            val keyPrefix = ByteBuffer.wrap(entry.getKey).getShort
            result.compute(
              keyPrefix,
              (_, maybePrev) =>
                maybePrev match {
                  case null => Stats(1, entry.getKey.length, entry.getValue.length)
                  case prev => Stats(prev.entryCount + 1, prev.totalKeySize + entry.getKey.length, prev.totalValueSize + entry.getValue.length)
              }
            )
          }
          iterator.close()

          log.info("key-space,entry-count,total-key-size,total-value-size")
          for ((prefix, stats) <- result.asScala) {
            log.info(s"${keys(prefix)},${stats.entryCount},${stats.totalKeySize},${stats.totalValueSize}")
          }

        case "TXBH" =>
          val txs = new ListBuffer[(TxNum, Transaction)]

          val h = Height(args(2).toInt)

          val prefix = ByteBuffer
            .allocate(6)
            .putShort(Keys.TransactionInfoPrefix)
            .putInt(h)
            .array()

          val iterator = db.iterator

          try {
            iterator.seek(prefix)
            while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) {
              val entry = iterator.next()

              val k = entry.getKey
              println(k.toList.map(_.toInt & 0xff))
              val v = entry.getValue

              for {
                idx <- Try(Shorts.fromByteArray(k.slice(6, 8)))
                tx  <- TransactionParsers.parseBytes(v)
              } txs.append((TxNum(idx), tx))
            }
          } finally iterator.close()

          println(txs.length)
          txs.foreach(println)

        case "AP" =>
          val address   = Address.fromString(args(2)).explicitGet()
          val portfolio = reader.portfolio(address)
          log.info(s"$address : ${portfolio.balance} WAVES, ${portfolio.lease}, ${portfolio.assets.size} assets")
          portfolio.assets.toSeq.sortBy(_._1.toString) foreach {
            case (assetId, balance) => log.info(s"$assetId : $balance")
          }

        case "APS" =>
          val addrs = mutable.Set[Address]()

          println(s"\nWAVES balances\n")
          for {
            (addressId, balance) <- db
              .iterateOverStream(Shorts.toByteArray(Keys.WavesBalancePrefix))
              .map { e =>
                val (_, addressId, _, height) = Keys.parseAddressBytesHeight(e.getKey)
                (addressId, height, Longs.fromByteArray(e.getValue))
              }
              .foldLeft(Map.empty[AddressId, Long]) { case (map, (addressId, _, balance)) => map + (addressId -> balance) }
            if balance > 0
          } yield {
            val addr = db.get(Keys.idToAddress(addressId))
            println(s"$addr : $balance")
            addrs += addr
          }

          val assets = mutable.ListBuffer[ByteStr]()
          db.iterateOver(10: Short) { e => // iterate over Keys.assetInfoHistory
            assets += ByteStr(e.getKey.drop(2))
          }

          println(s"\nAssets balances (${assets.size} assets)")
          for {
            assetId <- assets.sorted
            asset = IssuedAsset(assetId)
            seqNr <- {
              println(s"\n$assetId:")
              1 to db.get(Keys.addressesForAssetSeqNr(asset))
            }
            addressId <- db.get(Keys.addressesForAsset(asset, seqNr))
            balance = db
              .iterateOverStream(Bytes.concat(Shorts.toByteArray(Keys.AssetBalancePrefix), AddressId.toBytes(addressId)))
              .filter { e =>
                val (_, _, assetId, _) = Keys.parseAddressBytesHeight(e.getKey)
                ByteStr(assetId) == asset.id
              }
              .map(e => Longs.fromByteArray(e.getValue))
              .closeAfter(_.toStream.headOption)
              .getOrElse(0L)
            if balance > 0
          } yield {
            val addr = db.get(Keys.idToAddress(addressId))
            println(s"$addr : $balance")
            addrs += addr
          }

          println(s"\nAddress balances (${addrs.size} addresses)")
          addrs.toSeq.sortBy(_.toString).foreach { addr =>
            val p = reader.portfolio(addr)
            println(s"\n$addr : ${p.balance} ${p.lease}:")
            p.assets.toSeq.sortBy(_._1.id).foreach {
              case (IssuedAsset(assetId), bal) =>
                if (bal > 0)
                  println(s"$assetId : $bal")
            }
          }
      }
    } finally db.close()
  }
}
