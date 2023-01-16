import canoe.api._
import canoe.api.models.Keyboard
import canoe.models.InlineKeyboardButton.{callbackData, switchInlineQuery, switchInlineQueryCurrentChat, url}
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.messages.{AnimationMessage, StickerMessage, TelegramMessage, TextMessage}
import canoe.models._
import canoe.syntax._
import cats.Applicative
import cats.data.EitherT
import cats.effect.{Async, IO, IOApp, LiftIO, Sync}
import cats.free.Free
import cats.implicits.toTraverseOps
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import doobie.Transactor
import doobie.free.connection
import doobie.implicits._
import fs2.{Pipe, Stream}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Random, Try}

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = "5668930687:AAEzCyL4Y-cQoLph4EpW_y_7JX7c4SMA9TQ"
  val storage: Map[Long, String] = Map(0L -> "test")
  val numbers: Regex = "[1-9]".r
  val buf: ListBuffer[Sticker] = scala.collection.mutable.ListBuffer.empty[Sticker]
  val mp = scala.collection.mutable.Map.empty[Long, String]

  def run: IO[Unit] = for {
    transactor <- IO(Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:8888/testdb",
      "postgres",
      "123456"
    ))
    result <- Stream
      .resource(TelegramClient[IO](token))
      .flatMap(implicit client => Bot.polling[IO].follow(echos(transactor)).through(answerCallbacks(transactor)))
      .compile
      .drain
  } yield result

  def echos[F[_] : TelegramClient : Sync](trans: Transactor[F]): Scenario[F, Unit] =
    for {
      msg <- Scenario.expect(any)
      _ <- Scenario.eval(echoBack(msg, trans))
    } yield ()

  def echoBack[F[_] : TelegramClient : Sync](msg: TelegramMessage, trans: Transactor[F]): F[Unit] = msg match {
    case textMessage: TextMessage => textMessage.text match {
      case "database" =>
        for {
          unwrapGet <- getDB(trans)
          _ <- msg.chat.send("@" + s"${unwrapGet._2F.head}").void
        } yield ()

      case "deldb" => delAllDB(trans).void
      case "selfdel" => delFromRPS(374489132L, trans).void
      case "showl" => for {
        ss <- getDB(trans)
        _ <- msg.chat.send(s"$ss").void
      } yield ()
      case "clearrps" => delRpsDB(trans).void
      case "gamestat" => for {
        stat <- rpsStat(trans)
        _ <- msg.chat.send(s"$stat").void
      } yield ()

      //      case "rpslb" => for {
      //        res <- rpsLB(trans)
      //        _ <- msg.chat.send(res).void
      //      } yield ()
      case "/hello" => msg.chat.send("Hi").void
      case "/roll" => msg.chat.send("Random", keyboard = rollingBtn).void
      case "/random" => msg.chat.send(s"${Random.nextInt(6)}").void
      case "/reply" => msg.chat.send("ReplyTest", keyboard = replyBtn).void
      case "/forward" => msg.chat.send("ForwardTest", keyboard = forwardBtn).void
      case "/link" => msg.chat.send("LinksTest", keyboard = linksBtn).void
      case "/pay" => msg.chat.send("Donation", keyboard = pay).void
      case "/game" => msg.chat.send("Камень-Ножницы-Бумага", keyboard = rpsStart).void
      case "Выбирай" => msg.chat.send("Выбирай", keyboard = rpsGameBtn).void
      case _ => msg.chat.send("ti huy").void
    }

    case animationMessage: AnimationMessage => msg.chat.send(animationMessage.animation).void
    case stickerMessage: StickerMessage => msg.chat.send(stickerMessage.sticker).void
    case _ => msg.chat.send("Sorry! I can't echo that back.").void

  }

  def getDB[F[_] : TelegramClient : Sync](trans: Transactor[F]): F[List[(String, String)]] = {
    val request = sql"select pidor_id, name from testshema.pidordnya"
      .query[(String, String)]
      .to[List]
    val get: F[List[(String, String)]] = request.transact(trans)
    get
  }

  def answerCallbacks[F[_] : Sync : TelegramClient](trans: Transactor[F]): Pipe[F, Update, Update] =
    _.evalTap {
      case CallbackButtonSelected(_, query) =>
        query.data match {
          case Some(cbd) =>
            val id = query.from.id
            val name = if (query.from.username.isEmpty) {
              Try(query.from.firstName).get
            }
            else query.from.username.get
            cbd match {
              case "Добавлено" =>
                addToMap(id, name)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                  _ <- addToDB(id, name, trans)
                } yield ()
              case "Удалено" =>
                removeFromMap(id, name)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                  _ <- delFromDB(id, trans)
                } yield ()
              case "Рандом" =>
                for {
                  _ <- query.message.traverse(_.chat.send(random()))
                } yield ()
              case "Список" =>
                for {
                  _ <- query.message.traverse(_.chat.send(showList()))
                } yield ()

              case "Рега" => rpsStat(trans).flatMap {
                z =>
                  if (z.map(_._2).contains(id))
                    for {
                      _ <- query.message.traverse(_.chat.send("Выбирай", keyboard = rpsGameBtn))
                    } yield ()
                  else
                    for {
                      _ <- rpsReg(name, id, trans)
                      _ <- query.message.traverse(_.chat.send("Выбирай", keyboard = rpsGameBtn))
                    } yield ()
              }

              case "Камень" =>

                for {
                  res <- rps(id, cbd, trans)
                  _ <- query.message.traverse(_.chat.send(res, keyboard = rpsGameBtn))
                } yield ()
              case "Ножницы" =>

                for {
                  res <- rps(id, cbd, trans)
                  _ <- query.message.traverse(_.chat.send(res, keyboard = rpsGameBtn))
                } yield ()
              case "Бумага" =>

                for {
                  res <- rps(id, cbd, trans)
                  _ <- query.message.traverse(_.chat.send(res, keyboard = rpsGameBtn))
                } yield ()

              case "Стата" => rpsStat(trans).flatMap {
                z =>
                  if (z.map(_._2).contains(id)) {
                    for {
                      res <- rpsSelf(id, trans)
                      _ <- query.message.traverse(_.chat.send(s"Игрок @${res.head._1} \nПобеды: ${res.head._3} \nПоражениия: ${res.head._4}"))
                    } yield ()
                  }

                  else {
                    for {
                      _ <- query.message.traverse(_.chat.send("Ты еще слиток", keyboard = rpsStart)).void
                    } yield ()
                  }
              }


              case "Топ10" => {
                for {
                  stat <- rpsStat(trans)
                  name = stat.sortBy(_._3).take(10).reverse.zipWithIndex.foldLeft("") {
                    case (acc, ((a, b, c), num)) => s"$acc\n${num + 1}. @$a - $c"
                  }
                  _ <- query.message.traverse(_.chat.send(name))
                } yield ()
              }

              case "Слиток" => rpsStat(trans).flatMap {
                z =>
                  if (z.map(_._2).contains(id)) {
                    for {
                      del <- delFromRPS(id, trans)
                      _ <- query.message.traverse(_.chat.send("Слиток", keyboard = rpsStart)).void
                    } yield ()
                  }
                  else {
                    for {
                      _ <- query.message.traverse(_.chat.send("Ты уже слиток", keyboard = rpsStart)).void
                    } yield ()
                  }

              }
              case _ =>
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                } yield ()
            }
          case _ => Applicative[F].unit
        }
      case _ => Applicative[F].unit
    }

  def addToMap(id: Long, name: String): String = {
    mp.update(id, name)
    "Добавлено"
  }

  def addToDB[F[_] : Sync](id: Long, name: String, trans: Transactor[F]): F[Int] = {
    sql"insert into testshema.pidordnya values ($id, $name)"
      .update
      .run.transact(trans)
  }

  def delFromRPS[F[_] : Sync](id: Long, trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.rps_leaderboard where player_id = $id"
      .update
      .run.transact(trans)
  }

  def rpsStat[F[_] : Sync](trans: Transactor[F]): F[List[(String, Long, Long)]] = {

    val stat = sql"select player_name, player_id, win_counter from testshema.rps_leaderboard order by win_counter desc"
      .query[(String, Long, Long)]
      .to[List]
    val show: F[List[(String, Long, Long)]] = stat.transact(trans)
    show
  }

  def rpsSelf[F[_] : Sync](ID: Long, trans: Transactor[F]): F[List[(String, Long, Long, Long)]] = {

    val stat = sql"select * from testshema.rps_leaderboard where player_id = $ID"
      .query[(String, Long, Long, Long)]
      .to[List]
    val show: F[List[(String, Long, Long, Long)]] = stat.transact(trans)
    show
  }

  def rpsReg[F[_] : Sync](pName: String, pID: Long, trans: Transactor[F]): F[Int] = {
    sql"insert into testshema.rps_leaderboard (player_name, player_id) values ($pName, $pID)"
      .update
      .run.transact(trans)
  }

  def rps[F[_] : Sync](ID: Long, player: String, trans: Transactor[F]): F[String] = {
    val wc: F[Int] = for {
      winCount <- sql"select win_counter from testshema.rps_leaderboard where player_id = $ID"
        .query[Int]
        .to[List].transact(trans)
        .map(_.headOption.getOrElse(0))
    } yield winCount
    val lc: F[Int] = for {
      loseCount <- sql"select lose_counter from testshema.rps_leaderboard where player_id = $ID"
        .query[Int]
        .to[List].transact(trans)
        .map(_.headOption.getOrElse(0))
    } yield loseCount

    val r = "Камень"
    val p = "Бумага"
    val s = "Ножницы"
    val bot: List[String] = List(r, p, s)
    val botGet: String = bot(Random.nextInt(bot.length))
    player match {
      case win if (player == r && botGet == s) ||
        (player == p && botGet == r) ||
        (player == s && botGet == p) =>

        println("win_counter")
        for {
          wcc <- wc
          count = wcc + 1
          _ <- sql"update testshema.rps_leaderboard set win_counter = $count where player_id = $ID"
            .update
            .run.transact(trans)
          res = s"Игрок ($win) победил противника ($botGet)"
        } yield res


      case lose if (player == s && botGet == r) ||
        (player == r && botGet == p) ||
        (player == p && botGet == s) =>

        println("lose_counter")
        for {
          lcc <- lc
          count = lcc + 1
          _ <- sql"update testshema.rps_leaderboard set lose_counter = $count where player_id = $ID"
            .update
            .run.transact(trans)
          res = s"Противник ($botGet) победил игрока ($lose)"
        } yield res

      case _ if player == botGet => "Ничья".pure[F]
      case _ => "Пусто".pure[F]
    }
  }


  def delFromDB[F[_] : Sync](id: Long, trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.pidordnya where pidor_id = $id"
      .update
      .run.transact(trans)

  }

  def delAllDB[F[_] : Sync](trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.pidordnya"
      .update
      .run.transact(trans)
  }

  def delRpsDB[F[_] : Sync](trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.rps_leaderboard"
      .update
      .run.transact(trans)

  }

  def removeFromMap(id: Long, name: String): String = {
    if (mp.contains(id)) {
      mp.remove(id).get
      "Удалено"
    }
    else name + " не найден"

  }

  def showList(): String = {
    if (mp.isEmpty) "Пусто"
    else {
      val x: Seq[(Long, String)] = mp.toSeq
      val y = for {
        name <- x._2F
      } yield name
      y.mkString("Список: (", ", ", ")")
    }
  }

  def random(): String = {
    if (mp.isEmpty) "Пусто"
    else {
      val x: Seq[(Long, String)] = mp.toSeq
      val y = for {
        name <- x._2F
      } yield name
      "@" + y(Random.nextInt(y.length))
    }
  }

  def randomFromDB[F[_] : Sync](trans: Transactor[F]): F[List[String]] = {
    val request = sql"select name from testshema.pidordnya"
      .query[String]
      .to[List]
    val get = request.transact(trans)
    val x: F[List[String]] = for {
      s <- get
    } yield s
    val y: F[List[String]] = x
    y
  }

  val forward: InlineKeyboardButton = switchInlineQuery("test", "")
  val swInlineQ: InlineKeyboardMarkup = singleButton(forward)
  val forwardBtn: Keyboard.Inline = Keyboard.Inline(swInlineQ)

  val pay: Keyboard.Inline = Keyboard.Inline(
    InlineKeyboardMarkup.singleButton(
      callbackData("pay", "Work in progress...")))

  val buttontest: InlineKeyboardButton = switchInlineQueryCurrentChat("test", "")
  val swinlineQCC: InlineKeyboardMarkup = singleButton(buttontest)
  val replyBtn: Keyboard.Inline = Keyboard.Inline(swinlineQCC)


  val kbURL: InlineKeyboardButton = url("Google", "https://www.google.com/")
  val markup: InlineKeyboardMarkup = singleButton(kbURL)


  val links: List[List[InlineKeyboardButton]] = List(List(url("Google", "https://www.google.com/"),
    url("Youtube", "https://www.youtube.com/")),
    List(url("Pornhub", "https://rt.pornhub.com")))
  val linksBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(links))


  val kbtest: List[List[InlineKeyboardButton]] = List(List(callbackData("Добавить", "Добавлено"),
    callbackData("Удалить", "Удалено")),
    List(callbackData("Рандом", "Рандом"),
      callbackData("Список", "Список")))
  val rollingBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(kbtest))

  val rpsGame: List[List[InlineKeyboardButton]] = List(List(callbackData("Камень", "Камень"),
    callbackData("Ножницы", "Ножницы"),
    callbackData("Бумага", "Бумага")))
  val rpsGameBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(rpsGame))

  val rpsGameReg: List[List[InlineKeyboardButton]] = List(List(callbackData("Начать", "Рега"),
    callbackData("Статистика", "Стата")),
    List(callbackData("Топ 10", "Топ10"), callbackData("Покинуть игру", "Слиток")))

  val rpsStart: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(rpsGameReg))
}

