'use strict';

app.factory('constantService', [
  '$log', '$sce', 'constantData', 'messageData',
  function ($log, $sce, constants, messages) {

    messages.get = function (k) {
      if (typeof k === 'string')
        k = k.split('.');
      var m = this;
      for (var i = 0; m && i < k.length; i ++)
        m = m[k[i]];
      return m;
    };
    constants.messages = messages;

    constants.regex = {
      doi: /^(?:[dD][oO][iI]:|[hH][dD][lL]:|(?:https?:\/\/)?(dx\.)?doi\.org\/)?(10\.[0-9\.]+\/\S+)\s*$/,
    };

    //

    function invertArray(data) {
      for (var i = 0; i < data.length; i ++)
        data[data[i]] = i;
    }

    constants.releases = Object.keys(constants.release);
    constants.release[-1] = 'UNRELEASED';
    constants.releases.unshift('-1');
    invertArray(constants.permission);
    invertArray(constants.release);
    invertArray(constants.delivery);
    constants.categories = _.sortBy(constants.category, 'id');
    constants.categoryName = _.indexBy(constants.category, 'name');

    /* convenient aliases: */
    constants.permission.VIEW = constants.permission.PUBLIC;
    constants.permission.CONTRIBUTE = constants.permission.EDIT;
    constants.permission.SUPER = constants.permission.length;

    /* backwards compatibility: */
    for (var name in constants.party) {
      var uname = name.toUpperCase();
      var party = constants.party[name];
      if (typeof party === 'object' && name !== uname)
        constants.party[uname] = party.id;
    }

    var msgnot = messages.not;
    for (var c in constants.category) {
      var cat = constants.category[c];
      var n = cat.name;
      cat.not = n in msgnot ? msgnot[n] : 'No ' + n;
      var metrics = _.filter(constants.metric, 'category', cat.id);
      cat.metrics = _.sortBy(metrics, 'id');
      cat.metricName = _.indexBy(metrics, 'name');
      if (cat.metrics[0].type === 'void')
        cat.indicator = cat.metrics[0];
    }

    for (var f in constants.format) {
      var fmt = constants.format[f];
      var m = fmt.mimetype;
      fmt.type = m.slice(0, m.indexOf('/'));
    }

    constants.accessPreset = [
      [constants.permission.NONE,   constants.permission.NONE],
      [constants.permission.PUBLIC, constants.permission.SHARED],
      [constants.permission.PUBLIC, constants.permission.SHARED]
    ];
    constants.accessPreset.parties = [
      constants.party.NOBODY,
      constants.party.ROOT
    ];
    constants.accessPreset.staff = constants.permission.EDIT;

    constants.message = function (key /*, args...*/) {
      var msg = messages.get(key);

      if (msg === undefined) {
        $log.info('Message key [' + key + '] is undefined.');
        return '[' + key + ']';
      }

      var i = 1, l = arguments.length;
      var o;
      if (i < l && typeof arguments[i] === 'object')
        o = arguments[i++];
      else
        o = {};

      var g = o.sce ? $sce.getTrusted.bind($sce, o.sce) : angular.identity;
      for (var n = 0; i < l; i++, n++)
        msg = msg.replace('{' + n + '}', g(arguments[i]), 'g');
      if (o.sce)
        msg = $sce.trustAs(o.sce, msg);

      return msg;
    };

    constants.age = { day: 1, week: 7, year: 365.24219 };
    constants.age.month = constants.age.year / 12;
    constants.age.limit = constants.age.year * 90;

    function deepFreeze(o) {
      Object.freeze(o);
      for (var f in o)
        if (o.hasOwnProperty(f)) {
          var v = o[f];
          if (v instanceof Object && !Object.isFrozen(v))
            deepFreeze(v);
        }
      return o;
    }
    constants.deepFreeze = deepFreeze;

    return deepFreeze(constants);
  }
]);
