package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._

private[controllers] sealed class StaticController extends SiteController

object StaticHtml extends StaticController {
  def index() = SiteAction {
    implicit request =>
      Ok(views.html.widget.template.static("The Databrary Project")(views.html.static.index(request)))
  }

  def team() = SiteAction.async { implicit request =>
    for {
      v <- models.Volume.Databrary
      a <- v.partyAccess
    } yield (Ok(views.html.widget.template.static("Our Team")(views.html.static.team(a))))
  }

  def page(page : String) = SiteAction { implicit request =>
    views.html.static.pages.get(page).fold[SimpleResult](NotFound)(page =>
      Ok(views.html.widget.template.static(page.title)(page.template.render)))
  }
}

<<<<<<< HEAD
  object api {
    def index() = SiteAction {
      implicit request =>
        Ok(views.html.static.index(request))
    }

    def team() = SiteAction.async { implicit request =>
      for {
        v <- models.Volume.Databrary
        a <- v.partyAccess
      } yield (Ok(views.html.static.team(a)))
    }

    def page(page : String) = Action { implicit request =>
      views.html.static.pages.get(page).fold[SimpleResult](NotFound)(page =>
        Ok(page.template.render))
    }
=======
object StaticApi extends StaticController {
  def page(page : String) = Action { implicit request =>
    views.html.static.pages.get(page).fold[SimpleResult](NotFound)(page =>
      Ok(page.template.render))
>>>>>>> remotes/origin/master
  }
}
