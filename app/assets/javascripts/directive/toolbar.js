module.directive('toolbar', [
	'pageService', function (page) {
		var controller = function ($scope) {
			this.hoverUser = false;

			this.hideHover = function () {
				this.hoverUser = false;
			};

			//

			this.links = function () {
				return page.$filter('filter')(page.display.toolbarLinks, function (link) {
					return link.access && link.object ? page.auth.hasAccess(link.access, link.object) :
						link.auth ? page.auth.hasAuth(link.auth) : true;
				});
			};

			//

			this.stopProp = function ($event) { console.log(arguments);
				$event.stopImmediatePropagation();
				$event.stopPropagation();
			}
		};

		return {
			restrict: 'A',
			templateUrl: 'toolbar.html',
			replace: true,
			scope: true,
			controller: controller,
			controllerAs: 'toolbar',
		};
	}
]);
