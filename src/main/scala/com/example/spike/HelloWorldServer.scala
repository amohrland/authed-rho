package com.example.spike

import cats.effect.{Effect, IO}
import com.example.spike.authy.ConsumerUser
import fs2.StreamApp
import org.http4s.{AuthedService, HttpService}
import org.http4s.rho.AuthedContext
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.ExecutionContext

object HelloWorldServer extends StreamApp[IO] {
  import scala.concurrent.ExecutionContext.Implicits.global

  def stream(args: List[String], requestShutdown: IO[Unit]) = ServerStream.stream[IO]
}

object ServerStream {
  def rho[F[_]: Effect](swaggerSupport: SwaggerSupport[F], authedContext: AuthedContext[F,ConsumerUser]): HttpService[F] =
    new HelloWorldService[F].rhoService(authedContext).toService(swaggerSupport.createRhoMiddleware())

  def blah[F[_]: Effect](swaggerSupport: SwaggerSupport[F], authedContext: AuthedContext[F,ConsumerUser]): AuthedService[ConsumerUser, F] = {
    // Why is this called toService?
    authedContext.toService(rho(swaggerSupport, authedContext))
  }
  def vanilla[F[_]:Effect] = authy.authAdapter(authy.appAuth).apply(new HelloWorldService[F].authedService)

  def authedRhoService[F[_]:Effect] =
    authy.authAdapter(authy.appAuth).apply(blah(SwaggerSupport[F], new AuthedContext[F,ConsumerUser]()))

  def stream[F[_]: Effect](implicit ec: ExecutionContext) = {
    import cats.implicits._
    BlazeBuilder[F]
      .mountService(StaticContentService.routes[F].combineK(authedRhoService[F]), "/")
      .bindHttp()
      .serve
  }
}
