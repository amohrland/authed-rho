package com.example.spike

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import cats.data.{EitherT, Kleisli, OptionT}
import cats.effect._
import io.circe.Json
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.{AuthedContext, RhoService}
import org.http4s.server.AuthMiddleware
import org.http4s.{AuthedRequest, AuthedService, HttpService, Request, Response, StaticFile, Status}

object authy {

  case class ConsumerUser(userId: String)

  case class EnterpriseUser(userId: String)

  case class Err(message: String)

  type AppAuth[F[_], A] = Kleisli[F, Request[F], Either[Err, A]]

  def appAuth[F[_] : Applicative]: AppAuth[F,ConsumerUser] = Kleisli { r =>
    r.params
      .get("works")
      .flatMap(x => if (x === "true") Option(ConsumerUser("my_consumer_id_2")) else None)
      .toRight(Err(""))
      .pure[F]
  }

  def authAdapter[F[_]: Monad](appAuth: AppAuth[F,ConsumerUser]): AuthMiddleware[F, ConsumerUser] = {
    val k: Kleisli[OptionT[F, ?], Request[F], ConsumerUser] = Kleisli { req: Request[F] =>
      OptionT(appAuth.run(req).map(_.toOption))
    }
    AuthMiddleware(k)
  }

}

import com.example.spike.authy._

object StaticContentService {
  import org.http4s.dsl.io._
  private val swaggerUiDir = "/swagger-ui"

  def fetchResource[F[_]:Effect](path: String, req: Request[F]): F[Response[F]] = {
    StaticFile.fromResource(path, Some(req)).getOrElse(Response(Status.NotFound))
  }

  /**
    * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained
    */
  def routes[F[_]:Effect]: HttpService[F] = HttpService {
    // Swagger User Interface
    case req @ GET -> Root / "css" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "images" / _    => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "lib" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "swagger-ui"    => fetchResource(swaggerUiDir + "/index.html", req)
    case req @ GET -> Root / "swagger-ui.js" => fetchResource(swaggerUiDir + "/swagger-ui.min.js", req)
  }
}

case class HelloWorldService[F[_] : Effect]() {
  def rhoService(authedContext: AuthedContext[F,ConsumerUser]): RhoService[F] = new RhoService[F] with SwaggerSyntax[F] {
    "hello service" **
      GET / "hello" / pathVar[String] >>> authedContext.auth |>> {
      (req: Request[F], name: String, consumerUser: ConsumerUser) =>
        Ok(Json.obj("message" -> Json.fromString(s"Hello, ${name}. Your id is: ${consumerUser.userId}")))
    }
  }


  def authedService: AuthedService[ConsumerUser,F] = {
    val x = new Http4sDsl[F] {}
    import x._
    AuthedService[ConsumerUser, F] {
      case GET -> Root / "hello" / name as consumerUser =>
        Ok(Json.obj("message" -> Json.fromString(s"Hello, ${name}; Your id is ${consumerUser.userId}")))
    }
  }
}
