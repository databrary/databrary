'use strict'

app.directive 'partyEditNotifications', [
  'constantService', 'routerService', 'messageService',
  (constants, router, messages) ->
    restrict: 'E'
    templateUrl: 'party/editNotifications.html'
    link: ($scope) ->
      Notice = constants.notice
      Delivery = constants.delivery
      options = [Delivery.none, Delivery.site, Delivery.weekly, Delivery.daily, Delivery.async]

      $scope.notifications = [
        {
          name: "Account changes"
          subgroup: [
            {
              name: "Your email or password changed"
              notice: [Notice.AccountChange]
              options: [Delivery.async]
            }
          ]
        }, {
          name: "Your authorization status"
          subgroup: [
            {
              name: "You submitted an authorization request"
              notice: [Notice.AuthorizeRequest]
              options: [Delivery.site, Delivery.async]
            }, {
              name: "You were authorized"
              notice: [Notice.AuthorizeGranted]
              options: [Delivery.site, Delivery.weekly, Delivery.daily, Delivery.async]
            }, {
              name: "Your authorization is expiring"
              notice: [Notice.AuthorizeExpiring, Notice.AuthorizeExpired]
              options: [Delivery.site, Delivery.weekly, Delivery.daily]
            }
          ]
        }, {
          name: "Your affiliates"
          subgroup: [
            {
              name: "Someone requested authorization"
              notice: [Notice.AuthorizeChildRequest]
              options: [Delivery.async]
            }, {
              name: "You authorized someone"
              notice: [Notice.AuthorizeChildGranted]
              options: options
            }, {
              name: "Your affiliate's authorization is expiring"
              notice: [Notice.AuthorizeChildExpiring, Notice.AuthorizeChildExpired]
              options: [Delivery.none, Delivery.site, Delivery.weekly, Delivery.daily]
            }
          ]
        }, {
          name: "Your volume activity"
          subgroup: [
            {
              name: "An assistance request was submitted for your volume"
              notice: [Notice.VolumeAssist]
              options: options
            }, {
              name: "Someone created a volume on your behalf"
              notice: [Notice.VolumeCreated]
              options: options
            }, {
              name: "Someone changed the access permissions on your volume"
              notice: [Notice.VolumeSharing, Notice.VolumeAccessOther, Notice.VolumeAccess]
              options: options
            }, {
              name: "Someone marked data on your volume publicly releasable"
              notice: [Notice.ReleaseSlot, Notice.ReleaseAsset, Notice.ReleaseExcerpt]
              options: options
            }, {
              name: "Someone created a highlight on your volume"
              notice: [Notice.ExcerptVolume]
              options: options
            }, {
              name: "Someone commented on your volume or replied to your comment"
              notice: [Notice.CommentVolume, Notice.CommentReply]
              options: options
            }, {
              name: "Someone added a tag on your volume"
              notice: [Notice.TagVolume]
              options: options
            }
          ]
        }, {
          name: "Sitewide activity"
          subgroup: [
            {
              name: "A new volume was shared"
              notice: [Notice.SharedVolume]
              options: [Delivery.none, Delivery.site, Delivery.weekly, Delivery.daily]
            }
          ]
        }, {
          name: "Newsletter subscription",
          subgroup: [
            {
              name: "Subscribe to the newsletter"
              notice: [Notice.Newsletter]
              options: [Delivery.none, Delivery.async]
            }
          ]
        }
      ]
      $scope.delivery = (constants.message('notice.delivery.'+n) for n in constants.delivery)

      delivery = undefined
      getDelivery = (n) ->
        d = undefined
        for t in n.notice
          if d == undefined
            d = delivery[t]
          else if d != delivery[t]
            d = null
        n.delivery = d

      router.http(router.controllers.getNotify).then (res) ->
        delivery = res.data
        for nl in $scope.notifications
          for n in nl.subgroup
            getDelivery(n)
        return

      $scope.save = (n) ->
        $scope.partyEditNotificationsForm.$setSubmitted()
        d = n.delivery
        router.http(router.controllers.postNotify, {notice:n.notice, delivery:d}).then () ->
            for t in n.notice
              delivery[t] = d
            getDelivery(n)
            $scope.partyEditNotificationsForm.$setPristine()
            return
          , (res) ->
            $scope.partyEditNotificationsForm.$setUnsubmitted()
            messages.addError
              type: 'red'
              body: 'An error occurred saving your notification preferences'
              report: res
            getDelivery(n)
            return
        return

      return
]
