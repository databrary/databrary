package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json
import org.mindrot.jbcrypt.BCrypt
import macros._
import macros.async._
import dbrary._
import site._
import models._

sealed abstract class PartyController extends ObjectController[SiteParty] {
  /** ActionBuilder for party-targeted actions.
    * @param i target party id, defaulting to current user (site.identity)
    * @param p permission needed, or None if delegation is not allowed (must be self)
    */
  private[controllers] def action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    zip[Party.Id, Permission.Value, ActionFunction[SiteRequest.Base,Request]](i, p, (i, p) =>
      RequestObject.check[SiteParty](models.SiteParty.get(i)(_), p))
    .getOrElse {
      SiteAction.Auth ~> new ActionRefiner[SiteRequest.Auth,Request] {
        protected def refine[A](request : SiteRequest.Auth[A]) =
          if (i.forall(_ === request.identity.id))
            request.identity.perSite(request).map { p =>
              Right(request.withObj(p))
            }
          else
            async(Left(Forbidden))
      }
    }

  private[controllers] def Action(i : Option[models.Party.Id], p : Option[Permission.Value] = Some(Permission.ADMIN)) =
    SiteAction ~> action(i, p)

  protected def AdminAction(i : models.Party.Id, delegate : Boolean = true) =
    SiteAction.Unlocked ~> action(Some(i), if (delegate) Some(Permission.ADMIN) else None)

  protected def adminAccount(implicit request : Request[_]) : Option[Account] =
    request.obj.party.account.filter(_ === request.identity || request.superuser)

  protected def editForm(implicit request : Request[_]) : PartyController.EditForm =
    adminAccount.fold[PartyController.EditForm](new PartyController.PartyEditForm)(new PartyController.AccountEditForm(_))

  protected def createForm(acct : Boolean)(implicit request : SiteRequest[_]) : PartyController.CreateForm =
    if (acct) new PartyController.AccountCreateForm else new PartyController.PartyCreateForm

  def update(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    val form = editForm._bind
    val party = request.obj.party
    for {
      _ <- party.change(
	name = form.name.get,
	orcid = form.orcid.get,
	affiliation = form.affiliation.get,
	duns = form.duns.get.filter(_ => request.access.member == Permission.ADMIN),
	url = form.url.get
      )
      _ <- form.accountForm foreachAsync { form =>
	form.checkPassword(form.account)
	form.orThrow.account.change(
	  email = form.email.get,
	  password = form.cryptPassword,
	  openid = form.openid.get)
      }
      _ <- form.avatar.get foreachAsync { file : form.FilePart =>
	val fmt = AssetFormat.getFilePart(file).filter(_.isImage) getOrElse
	  form.avatar.withError("file.format.unknown", file.contentType.getOrElse("unknown"))._throw
	request.obj.setAvatar(file.ref, fmt, Maybe(file.filename).opt)
      }
    } yield (result(request.obj))
  }

  def create(acct : Boolean = false) : Action[_] = SiteAction.rootAccess().async { implicit request =>
    val form = createForm(acct)._bind
    for {
      p <- Party.create(
	name = form.name.get.get,
	orcid = form.orcid.get.flatten,
	affiliation = form.affiliation.get.flatten,
	duns = form.duns.get.flatten,
	url = form.url.get.flatten)
      a <- cast[PartyController.AccountCreateForm](form).mapAsync(form =>
	Account.create(p,
	  email = form.email.get.get,
	  password = form.cryptPassword,
	  openid = form.openid.get.flatten)
      )
      s <- p.perSite
    } yield (result(s))
  }

  def authorizeChange(id : models.Party.Id, childId : models.Party.Id) =
    AdminAction(id).async { implicit request =>
      models.Party.get(childId).flatMap(_.fold(ANotFound) { child =>
	val form = new PartyController.AuthorizeChildForm(child)._bind
	if (form.delete.get)
	  models.Authorize.delete(childId, id)
	    .map(_ => result(request.obj))
	else for {
	  c <- Authorize.get(child, request.obj.party)
	  _ <- Authorize.set(childId, id,
	    form.site.get,
	    form.member.get,
	    form.expires.get.map(_.toLocalDateTime(new org.joda.time.LocalTime(12, 0))))
	  _ <- Authorize.Info.set(childId, id, form.info.get)
	  _ <- async.when(Play.isProd && !c.exists(_.authorized),
	    Mail.send(
	      to = child.account.map(_.email).toSeq :+ Mail.authorizeAddr,
	      subject = Messages("mail.authorized.subject"),
	      body = Messages("mail.authorized.body", request.obj.party.name)))
	} yield (result(request.obj))
      })
    }

  def authorizeDelete(id : models.Party.Id, other : models.Party.Id) = AdminAction(id).async { implicit request =>
    for {
      /* users can remove themselves from any relationship */
      _ <- models.Authorize.delete(id, other)
      _ <- models.Authorize.delete(other, id)
    } yield (result(request.obj))
  }

  private def delegates(party : Party) : Future[Seq[Account]] =
    party.authorizeChildren().map(_.filter(_.permission >= Permission.ADMIN).flatMap(_.child.account)
      ++ party.account)

  def authorizeApply(id : models.Party.Id, parentId : models.Party.Id) = AdminAction(id).async { implicit request =>
    models.Party.get(parentId).flatMap(_.fold(ANotFound) { parent =>
    val form = new PartyController.AuthorizeApplyForm(parent)._bind
    for {
      _ <- Authorize.apply(id, parentId)
      _ <- Authorize.Info.set(id, parentId, form.info.get)
      dl <- delegates(parent)
      _ <- async.when(Play.isProd, Mail.send(
	to = dl.map(_.email) :+ Mail.authorizeAddr,
	subject = Messages("mail.authorize.subject"),
	body = Messages("mail.authorize.body", routes.PartyHtml.view(parentId).absoluteURL(Play.isProd),
	  request.obj.party.name + request.user.fold("")(" <" + _.email + ">"),
	  parent.name + form.info.get.fold("")(" (" + _ + ")"))
      ).recover {
	case ServiceUnavailableException => ()
      })
    } yield (result(request.obj))
    })
  }

  def authorizeSearch(id : models.Party.Id, apply : Boolean) =
    AdminAction(id).async { implicit request =>
      val form = new PartyController.AuthorizeSearchForm(apply)._bind
      if (form.notfound.get)
	for {
	  _ <- Mail.send(
	    to = Seq(Mail.authorizeAddr),
	    subject = Messages("mail.authorize.subject"),
	    body = Messages("mail.authorize.body", routes.PartyHtml.view(id).absoluteURL(Play.isProd),
	      request.obj.party.name + request.user.fold("")(" <" + _.email + ">") + request.obj.party.affiliation.fold("")(" (" + _ + ")"),
	      form.name.get + form.info.get.fold("")(" (" + _ + ")")))
	} yield (Ok("request sent"))
      else for {
	res <- models.Party.searchForAuthorize(form.name.get, request.obj.party, form.institution.get)
        r <- if (request.isApi) async(Ok(JsonRecord.map[Party](_.json)(res)))
	  else PartyHtml.viewAdmin(form +: res.map(e =>
	    (if (apply) new PartyController.AuthorizeApplyForm(e) else new PartyController.AuthorizeChildForm(e)).copyFrom(form)))
	    .map(Ok(_))
      } yield (r)
    }
}

object PartyController extends PartyController {
  private final val maxExpiration = org.joda.time.Years.years(2)

  abstract sealed class PartyForm(action : Call)(implicit request : SiteRequest[_])
    extends HtmlForm[PartyForm](action,
      views.html.party.edit(_)) {
    def actionName : String
    def formName : String = actionName + " Party"
    val name : Field[Option[String]]
    val orcid = Field(OptionMapping(Forms.optional(Forms.of[Orcid])))
    val affiliation = Field(OptionMapping(Mappings.maybeText))
    val duns = Field(OptionMapping(Forms.optional(Forms.of[DUNS])))
    val url = Field(OptionMapping(Forms.optional(Forms.of[java.net.URL])))
  }
  sealed trait AccountForm extends PartyForm with LoginController.PasswordChangeForm {
    override def formName : String = actionName + " Account"
    val email : Field[Option[String]]
    val password = passwordField.fill(None)
    val openid = Field(OptionMapping(Forms.optional(Forms.of[java.net.URL])))
  }

  abstract sealed class EditForm(implicit request : Request[_])
    extends PartyForm(routes.PartyHtml.update(request.obj.id)) {
    def actionName = "Update"
    override def formName = "Edit Profile"
    def party = request.obj.party
    def accountForm : Option[AccountEditForm]
    val name = Field(OptionMapping(Mappings.nonEmptyText)).fill(Some(party.name))
    val avatar = OptionalFile()
    orcid.fill(Some(party.orcid))
    affiliation.fill(Some(party.affiliation))
    duns.fill(if (request.access.member >= Permission.ADMIN) Some(party.duns) else None)
    url.fill(Some(party.url))
  }
  final class PartyEditForm(implicit request : Request[_]) extends EditForm {
    def accountForm = None
  }
  final class AccountEditForm(val account : Account)(implicit request : Request[_]) extends EditForm with AccountForm with LoginController.AuthForm {
    def accountForm = if (_authorized || request.superuser) Some(this)
      else if (email.get.exists(!_.equals(account.email)) || password.get.nonEmpty || openid.get.exists(!_.equals(account.openid)))
	Some(this.auth.withError("error.required"))
      else None
    val email = Field(OptionMapping(Forms.email)).fill(Some(account.email))
    openid.fill(Some(account.openid))
  }

  abstract sealed class CreateForm(implicit request : SiteRequest[_])
    extends PartyForm(routes.PartyHtml.create()) {
    def actionName = "Create"
    val name = Field(Mappings.some(Mappings.nonEmptyText))
  }
  final class PartyCreateForm(implicit request : SiteRequest[_]) extends CreateForm
  final class AccountCreateForm(implicit request : SiteRequest[_]) extends CreateForm with AccountForm {
    val email = Field(Mappings.some(Forms.email))
  }
  sealed trait AuthorizeBaseForm extends StructForm {
    val info = Field(Forms.optional(Forms.nonEmptyText)).fill(None)
    def copyFrom(f : AuthorizeForm) : this.type = {
      info.fill(f.info.get)
      this
    }
  }

  sealed trait AuthorizeFullForm extends AuthorizeBaseForm {
    val site = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val member = Field(Forms.default(Mappings.enum(Permission), Permission.NONE))
    val delete = Field(Forms.boolean).fill(false)
    val expires = Field(Forms.optional(Forms.jodaLocalDate))
    private[controllers] def _fill(auth : Authorize) : this.type = {
      site.fill(auth.site)
      member.fill(auth.member)
      expires.fill(auth.expires.map(_.toLocalDate))
      info.fill(auth.info)
      this
    }
  }
  sealed abstract class AuthorizeForm(action : Call)(implicit request : Request[_])
    extends AHtmlForm[AuthorizeForm](action,
      f => PartyHtml.viewAdmin(Seq(f)))
    with AuthorizeBaseForm {
    def _apply : Boolean
  }
  sealed trait AuthorizeOtherForm extends AuthorizeForm {
    def targetParty : Party
  }
  final class AuthorizeChildForm(val child : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeChange(request.obj.id, child.id))
    with AuthorizeFullForm
    with AuthorizeOtherForm {
    def targetParty = child
    def _apply = false
    private[this] val maxexp = (new Date).plus(maxExpiration)
    override val expires = Field(if (request.superuser) Forms.optional(Forms.jodaLocalDate)
      else Mappings.some(Forms.default(Forms.jodaLocalDate, maxexp)
	.verifying(validation.Constraint[Date]("constraint.max", maxExpiration) { d =>
	  if (d.isAfter(maxexp)) validation.Invalid(validation.ValidationError("error.max", maxExpiration))
	  else validation.Valid
	}), maxexp))
    private[controllers] override def _fill(auth : Authorize) : this.type = {
      assert(request.obj === auth.parent)
      assert(child === auth.child)
      super._fill(auth)
    }
  }
  final class AuthorizeApplyForm(val parent : Party)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeApply(request.obj.id, parent.id))
    with AuthorizeOtherForm {
    def targetParty = parent
    def _apply = true
  }
  final class AuthorizeSearchForm(val _apply : Boolean)(implicit request : Request[_])
    extends AuthorizeForm(routes.PartyHtml.authorizeSearch(request.obj.id, _apply)) {
    val name = Field(Mappings.nonEmptyText)
    val institution = Field(OptionMapping(Forms.boolean)).fill(None)
    val notfound = Field(Forms.boolean).fill(false)
  }
  final class AuthorizeAdminForm(val authorize : Authorize)(implicit request : SiteRequest[_])
    extends StructForm(routes.PartyHtml.authorizeChange(authorize.parentId, authorize.childId))
    with AuthorizeFullForm {
    _fill(authorize)
  }
}

object PartyHtml extends PartyController with HtmlController {
  import PartyController._

  def viewParty(implicit request : Request[_]) = 
    for {
      parents <- request.obj.party.authorizeParents()
      children <- request.obj.party.authorizeChildren()
      vols <- request.obj.volumeAccess
      comments <- request.obj.party.account.fold[Future[Seq[Comment]]](async(Nil))(_.comments)
    } yield (Ok(views.html.party.view(parents, children, vols, comments)))

  def profile =
    (SiteAction.Unlocked ~> action(None, Some(Permission.NONE))).async(viewParty(_))

  def view(i : models.Party.Id) =
    Action(Some(i), Some(Permission.NONE)).async(viewParty(_))

  private[controllers] def viewAdmin(
    authorizeForms : Seq[AuthorizeForm] = Nil)(
    implicit request : Request[_]) = {
    val change = authorizeForms.collect { case o : AuthorizeOtherForm => o.targetParty.id.unId }.toSet
    val search = Set(false, true) -- authorizeForms.collect { case a : AuthorizeSearchForm => a._apply }
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
      forms = children
        .filterNot(t => change.contains(t.childId.unId))
        .map(t => new AuthorizeChildForm(t.child)._fill(t)) ++
	search.map(new AuthorizeSearchForm(_)) ++
        authorizeForms
    } yield (views.html.party.authorize(parents, forms))
  }
  
  def edit(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    editForm.Ok
  }

  def createNew(acct : Boolean = false) = SiteAction.rootAccess().async { implicit request =>
    createForm(acct).Ok
  }

  def admin(i : models.Party.Id) = AdminAction(i).async { implicit request =>
    viewAdmin().map(Ok(_))
  }

  def authorizeAdmin = SiteAction.rootAccess().async { implicit request =>
    for {
      all <- Authorize.getAll
      (rest, pend) = all.partition(_.authorized)
      (act, exp) = rest.partition(_.valid)
      part <- Party.getAll
    } yield (Ok(views.html.party.authorizeAdmin(part, pend.map(new AuthorizeAdminForm(_)), act, exp)))
  }

  def avatar(i : models.Party.Id, size : Int = 64) =
    (SiteAction.Unlocked ~> action(Some(i), Some(Permission.NONE))).async { implicit request =>
      request.obj.avatar.flatMap(_.fold(
	async(Found("//gravatar.com/avatar/"+request.obj.party.account.fold("none")(a => store.MD5.hex(a.email.toLowerCase))+"?s="+size+"&d=mm")))(
	AssetController.assetResult(_)))
    }

}

object PartyApi extends PartyController with ApiController {
  def get(partyId : models.Party.Id) = Action(Some(partyId), Some(Permission.NONE)).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }

  def profile =
    (SiteAction.Unlocked ~> action(None, Some(Permission.NONE))).async { implicit request =>
      request.obj.json(request.apiOptions).map(Ok(_))
    }

  final class SearchForm(implicit request : SiteRequest[_])
    extends ApiForm(routes.PartyApi.query) {
    val query = Field(Mappings.maybeText)
    val access = Field(OptionMapping(Mappings.enum(Permission)))
  }

  def query = SiteAction.Unlocked.async { implicit request =>
    val form = new SearchForm()._bind
    Party.search(form.query.get, form.access.get).map(l =>
      Ok(JsonRecord.map[Party](_.json)(l)))
  }

  def authorizeGet(partyId : models.Party.Id) = AdminAction(partyId).async { implicit request =>
    for {
      parents <- request.obj.party.authorizeParents(true)
      children <- request.obj.party.authorizeChildren(true)
    } yield (Ok(JsonObject(
      'parents -> JsonRecord.map[Authorize](a => JsonRecord(a.parentId,
        'party -> a.parent.json) ++
        a.json)(parents),
      'children -> JsonRecord.map[Authorize](a => JsonRecord(a.childId,
        'party -> a.child.json) ++
        a.json)(children)
      ).obj))
  }
}
