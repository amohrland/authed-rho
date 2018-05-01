package com.example.spike

import cats.effect.{Effect, IO}
import com.example.spike.authy.{CUser, EUser}
import fs2.StreamApp
import org.http4s.{AuthedService, HttpService}
import org.http4s.rho.{AuthedContext, RhoService}
import org.http4s.rho.swagger.{SwaggerSupport, models}
import org.http4s.server.AuthMiddleware
import org.http4s.server.blaze.BlazeBuilder
import cats.implicits._
import cats._
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import shapeless._

import scala.concurrent.ExecutionContext

object HelloWorldServer extends StreamApp[IO] {
  import scala.concurrent.ExecutionContext.Implicits.global

  def stream(args: List[String], requestShutdown: IO[Unit]) = ServerStream.stream[IO]
}

object ServerStream {
  def authedEHttpService[F[_]: Effect](eRhoService: RhoService[F], eAuthedContext: AuthedContext[F,EUser]): HttpService[F] = {
    val eTempService: HttpService[F] = eRhoService.toService()
    val eAuthedService: AuthedService[EUser, F] = eAuthedContext.toService(eTempService)
    val eAuthMiddlware: AuthMiddleware[F, EUser] = authy.toAuthMiddleware[F,EUser](authy.eUserAuth)
    eAuthMiddlware(eAuthedService)
  }

  def swaggerHttpService[F[_]:Effect](swaggerSupport: SwaggerSupport[F], rhoService: RhoService[F]): HttpService[F] = {
    val comboSwagger: models.Swagger = swaggerSupport.createSwagger()(rhoService.getRoutes)
    val swaggerRhoService: RhoService[F] = swaggerSupport.createSwaggerRoute(comboSwagger)
    swaggerRhoService.toService()
  }

  def authedCHttpService[F[_]:Effect](cRhoService: RhoService[F], cAuthedContext: AuthedContext[F,CUser]): HttpService[F] = {
    val cTempService: HttpService[F] = cRhoService.toService()
    val cAuthedService: AuthedService[CUser, F] = cAuthedContext.toService(cTempService)
    val cAuthMiddlware: AuthMiddleware[F, CUser] = authy.toAuthMiddleware[F,CUser](authy.cUserAuth)
    cAuthMiddlware(cAuthedService)
  }

  def stream[F[_]: Effect](implicit ec: ExecutionContext) = {
    val cMountPath = "c"
    val eMountPath = "e"
    val swaggerSupport = SwaggerSupport[F]
    val eAuthedContext = new AuthedContext[F,EUser]()
    val cAuthedContext = new AuthedContext[F,CUser]()
    val cRhoService: RhoService[F] = new CService[F].rhoService(cAuthedContext)
    val eRhoService: RhoService[F] = new EService[F].rhoService(eAuthedContext)

    val prefixedERhoService: RhoService[F] = TypedPath[F,shapeless.HNil](PathMatch(eMountPath)) /: eRhoService
    val prefixedCRhoService: RhoService[F] = TypedPath[F,shapeless.HNil](PathMatch(cMountPath)) /: cRhoService

    val swagHttpService: HttpService[F] =
      swaggerHttpService(swaggerSupport, prefixedCRhoService.and(prefixedERhoService))
      .combineK(StaticContentService.routes[F])

    val authedEHttpSvc: HttpService[F] = authedEHttpService(eRhoService, eAuthedContext)
    val authedCHttpSvc: HttpService[F] = authedCHttpService(cRhoService, cAuthedContext)

    BlazeBuilder[F]
      .mountService(swagHttpService, "/")
      .mountService(authedEHttpSvc, "/" + eMountPath + "/")
      .mountService(authedCHttpSvc, "/" + cMountPath + "/")
      .bindHttp()
      .serve
  }
}
