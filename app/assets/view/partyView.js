'use strict';

module.controller('partyView', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    $scope.volumes = party.volumes.map(function (va) {
      return va.volume;
    });

    page.display.title = party.name;
    if (page.models.Login.checkAccess(page.permission.EDIT, party))
      page.display.toolbarLinks.push({
        type: 'yellow',
        html: page.constants.message('party.edit'),
        url: party.editRoute(),
      });
  }
]);
