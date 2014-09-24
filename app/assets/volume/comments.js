'use strict';

module.controller('volume/comments', [
  '$scope', 'pageService', '$sanitize',
  function ($scope, page, $sanitize) {
    $scope.canPost = page.models.Login.isAuthorized();

    $scope.refreshPanel = function () {
      $scope.comments = $scope.volume.comments;
      $scope.enabled = $scope.canPost || !$.isEmptyObject($scope.comments);
    };

    //

    $scope.pullComments = function () {
      $scope.volume.get(['comments']).then(
        $scope.refreshPanel,
        function (res) {
          page.messages.addError({
            body: page.constants.message('comments.update.error'),
            report: res,
          });
        });
    };

    //

    $scope.commentMeta = function (comment) {
      return '<time datetime="' + page.$filter('date')(comment.time, 'yyyy-MM-dd HH:mm:ss Z') + '" pubdate>' + page.$filter('date')(comment.time, 'MMMM d, yyyy') + '</time>';
    };

    $scope.commentClass = function (comment) {
      var cls = {};
      if (comment.parents)
        cls['depth-'+Math.min(comment.parents.length, 5)] = true;
      return cls;
    };

    //

    $scope.replyTo = undefined;

    $scope.setReply = function (comment) {
      $scope.replyTo = comment;
    };

    //

    page.events.listen($scope, 'commentReplyForm-init', function (event, form) {
      form.successFn = $scope.pullComments;
      form.cancelFn = $scope.setReply;
      form.target = $scope.replyTo;
      event.stopPropagation();
    });

  }
]);
