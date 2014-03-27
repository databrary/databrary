define(['config/module'], function (module) {
	'use strict';

	module.factory('RouterService', ['$rootScope', '$route', '$filter', 'TypeService', function ($rootScope, $route, $filter, type) {
		var routerService = {};

		routerService.$route = $route;

		//

		routerService.makeUrl = function (url, params, stripQuery) {
			if (!params) return url;

			if (!angular.isObject(params) || angular.isArray(params))
				throw new Error('RouterService.makeUrl passed non-object "params" for ' + url);

			var parts = [];

			angular.forEach(params, function (value, key) {
				if (value === null || angular.isUndefined(value))
					return;

				if (!angular.isArray(value))
					value = [value];

				angular.forEach(value, function (v) {
					if (angular.isObject(v))
						v = angular.toJson(v);

					var regex = new RegExp(':' + key + '\\*?'),
						match = url.match(regex);

					if (match != null && match[0].length > 0)
						url = url.replace(regex, v);
					else
						parts.push($filter('uri')(key, true) + '=' +
							$filter('uri')(v, true));
				});
			});

			if (stripQuery !== false && parts.length > 0)
				url += ((url.indexOf('?') == -1) ? '?' : '&');

			return url + parts.join('&');
		};

		//

		var makeRoute = function (route) {
			return function (params, stripQuery) {
				return routerService.makeUrl(route, params, stripQuery);
			};
		};

		//

		routerService.index = makeRoute('/');
		routerService.login = makeRoute('/login');
		routerService.register = makeRoute('/register');

		routerService.search = makeRoute('/search');
		routerService.asset = makeRoute('/asset/:id');
		routerService.volume = makeRoute('/volume/:id');
		routerService.slotAsset = makeRoute('/slot/:sid/asset/:id');

		//

		routerService.record = function (data) {
			if (!type.isRecord(data))
				throw new Error('routerService.record() requires Record as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/record/:id', data);
		};

		routerService.slot = function (data) {
			if (!type.isSession(data))
				throw new Error('routerService.slot() requires Slot as first parameter');

			data = {
				id: data.id,
				segment: type.segmentString(data)
			};

			return routerService.makeUrl('/slot/:id', data);
		};

		routerService.volumeThumb = function (data) {
			if (!type.isVolume(data))
				throw new Error('routerService.volumeThumb() requires Volume as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/volume/:id/thumb', data);
		};

		routerService.slotThumb = function (data) {
			if (!type.isSession(data))
				throw new Error('routerService.slotThumb() requires Slot as first parameter');

			data = {
				id: data.id,
				segment: type.segmentString(data)
			};

			return routerService.makeUrl('/slot/:id/thumb', data);
		};

		routerService.assetThumb = function (data) {
			if (!type.isAsset(data))
				throw new Error('routerService.assetThumb() requires Asset as first parameter');

			data = {
				sid: data.container.id,
				id: data.asset.id,
				segment: type.segmentString(data)
			};

			return routerService.makeUrl('/slot/:sid/asset/:id/thumb', data);
		};

		routerService.assetHead = function (data) {
			if (!type.isAsset(data))
				throw new Error('routerService.assetHead() requires Asset as first parameter');

			data = {
				sid: data.container.id,
				id: data.asset.id,
				segment: type.segmentString(data)
			};

			return routerService.makeUrl('/slot/:sid/asset/:id/head', data);
		};

		routerService.assetLink = function (data, inline) {
			if (!type.isAsset(data))
				throw new Error('routerService.assetLink() requires Asset as first parameter');

			data = {
				sid: data.container.id,
				id: data.asset.id,
				segment: type.segmentString(data)
			};

			data.inline = data.inline || inline || false;

			return routerService.makeUrl('/slot/:sid/asset/:id/download', data);
		};

		routerService.partyAvatar = function (data, size) {
			if (!type.isParty(data))
				throw new Error('routerService.partyAvatar() requires Party as first parameter');

			data = {
				id: data.id
			};

			if (angular.isNumber(parseInt(size)))
				if (angular.isObject(data))
					data.size = parseInt(size);
				else if (angular.isArray(data))
					data.push(parseInt(size));
				else if (angular.isString(data))
					data = [data, size];
				else
					data = '';

			return routerService.makeUrl('/party/:id/avatar', data);
		};

		routerService.slotEdit = function (data) {
			if (!type.isSession(data))
				throw new Error('routerService.slotEdit() requires Slot as first parameter');

			data = {
				id: data.id,
				segment: type.segmentString(data)
			};

			return routerService.makeUrl('/slot/:id/edit', data);
		};

		routerService.assetEdit = function (data) {
			if (!type.isAsset(data))
				throw new Error('routerService.assetEdit() requires Asset as first parameter');

			data = {
				id: data.asset.id
			};

			return routerService.makeUrl('/asset/:id/edit', data);
		};

		routerService.recordEdit = function (data) {
			if (!type.isRecord(data))
				throw new Error('routerService.recordEdit() requires Record as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/record/:id/edit', data);
		};

		routerService.volumeEdit = function (data) {
			if (!type.isVolume(data))
				throw new Error('routerService.volumeEdit() requires Volume as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/volume/:id/edit', data);
		};

		routerService.volumeAccess = function (data) {
			if (!type.isVolume(data))
				throw new Error('routerService.volumeAccess() requires Volume as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/volume/:id/access', data);
		};

		routerService.partyEdit = function (data) {
			if (!type.isParty(data))
				throw new Error('routerService.partyEdit() requires Party as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/party/:id/edit', data);
		};

		routerService.party = function (data) {
			if (angular.isUndefined(data))
				return console.log(data);

			if (!type.isParty(data))
				throw new Error('routerService.party() requires Party as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/party/:id', data);
		};

		routerService.partyAuthorize = function (data) {
			if (!type.isParty(data))
				throw new Error('routerService.partyAuthorize() requires Party as first parameter');

			data = {
				id: data.id
			};

			return routerService.makeUrl('/party/:id/authorize', data);
		};

		//

		return routerService;
	}]);
});

