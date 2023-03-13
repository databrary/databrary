'use strict'

app.directive 'volumeAssist', [
  '$sce', 'constantService', 'uploadService', 'messageService', 'routerService',
  ($sce, constants, uploads, messages, router) ->
    restrict: 'E'
    templateUrl: 'volume/assist.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeAssist
      $flow = $scope.$flow # not really in this scope

      $scope.flowOptions = uploads.flowOptions()

      staffPerm = constants.accessPreset.staff
      updateStaff = () ->
        form.staff = !!volume.accessStaff
        return
      updateStaff()

      $scope.setStaff = (set) ->
        p = if set then staffPerm else 0
        form.$setSubmitted()
        volume.accessSave(constants.party.STAFF, if set then staffPerm else 0).then () ->
              form.$setPristine()
              updateStaff()
              return
          , (res) ->
              form.$setUnsubmitted()
              messages.addError
                body: constants.message('access.preset.save.error')
                report: res
                owner: form
              return

      slot = volume.top
      $scope.assets = (asset for a, asset of slot.assets)
      $scope.assets.sort((a,b) -> b.id-a.id)
      $scope.uploads = []
      $scope.progress = 0

      remove = (file) ->
        $scope.uploads.remove(file)
        form.uploads.$setPristine() unless $scope.uploads.length
        $scope.progress = file.flowObj.progress()
        return

      $scope.fileAdded = (file) ->
        $flow = file.flowObj
        $scope.uploads.unshift(file)
        form.uploads.$setDirty()
        uploads.upload(volume, file).then (res) ->
            file.progressValue = 0
            return
          , (res) ->
            remove(file)
            return
        return

      $scope.fileSuccess = (file) ->
        file.progressValue = 1
        slot.createAsset(
            upload: file.uniqueIdentifier
            name: file.relativePath
            position: null
            classification: null
          ).then (asset) ->
              $scope.assets.unshift(asset)
              file.cancel()
              remove(file)
              return
            , (res) ->
              messages.addError
                type: 'red'
                body: constants.message('asset.update.error', file.relativePath)
                report: res
                owner: form
              file.cancel()
              remove(file)
              return
        return

      $scope.fileProgress = (file) ->
        file.progressValue = file.progress()
        $scope.progress = file.flowObj.progress()
        return

      $scope.fileError = (file, message) ->
        messages.addError
          type: 'red'
          body: constants.message('asset.upload.error', file.relativePath, message || 'unknown error')
          owner: form
        file.cancel()
        remove(file)
        return

      $scope.remove = (asset) ->
        asset.remove().then (asset) ->
            $scope.assets.remove(asset)
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: constants.message('asset.remove.error', @name)
              report: res
              owner: this
            return
        return

      $scope.cancel = (file) ->
        file.cancel()
        remove(file)
        return

      form.questions =
        [
          name: 'released'
          text: "Are you using the Databrary release?"
        ,
          name: 'equivalent'
          text: "If not, are you using an equivalent consent/media release (see http://databrary.org/access/guide/investigators/release/grandfathering-data.html)?"
        ,
          name: 'complete'
          text: "Is data collection completed?"
        ,
          name: 'share'
          text: "Are you ready to share now?"
        ]

      form.submit = () ->
        body = ""
        for q in form.questions when q.answer
          body += q.text + " " + q.answer + "\n"
        body += "\n" + form.additional

        router.http(router.controllers.postVolumeAssist, [volume.id], body, {headers:{'Content-Type':"text/plain"}}).then () ->
              delete form.additional
              for q in form.questions
                delete q.answer
              form.mail.$setPristine()
              messages.add
                type: 'green'
                body: 'Your request has been submitted, and you will receive a copy by email'
                owner: form.mail
              return
          , (res) ->
              form.mail.$setUnsubmitted()
              messages.addError
                type: 'red'
                body: 'An error occurred sending the request; please try again'
                report: res
                owner: form.mail
              return
        return

      $scope.$on '$destroy', ->
        $flow?.cancel()
        return

      return
]
