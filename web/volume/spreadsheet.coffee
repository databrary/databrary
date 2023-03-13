'use strict'

app.directive 'spreadsheet', [
  'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', '$compile', '$templateCache', '$timeout', '$document', '$location', '$filter', '$route', 'routerService', 'storageService', 'Segment',
  (constants, display, messages, tooltips, styles, $compile, $templateCache, $timeout, $document, $location, $filter, $route, router, storage, Segment) ->
    restrict: 'E'
    scope: true
    templateUrl: 'volume/spreadsheet.html'
    controller: [
      '$scope', '$element', '$attrs',
      ($scope, $element, $attrs) ->
        volume = $scope.volume

        Editing = $scope.editing = $attrs.edit != undefined
        ID = $scope.id = $attrs.id ? 'ss'
        $scope.showGlobal = false
        Limit = $attrs.limit || Infinity
        Key = undefined

        maybeInt = (s) ->
          if isNaN(i = parseInt(s, 10)) then s else i
        byDefault = (a,b) -> +(a > b) || +(a == b) - 1
        byNumber = (a,b) -> a-b
        byType = (a,b) ->
          ta = typeof a
          tb = typeof b
          if ta != tb
            a = ta
            b = tb
          byDefault(a,b)
        byMagic = (a,b) ->
          if isNaN(d = a-b) then byType(a,b) else d
        bySortId = (a,b) ->
          (a.sort || a.id)-(b.sort || b.id)
        parseIntish = (c) ->
          if isNaN(i = parseInt(c, 10)) then c else i

        stripPrefix = (s, prefix) ->
          if s.startsWith(prefix) then s.substr(prefix.length)

        # autovivification
        arr = (a, f) ->
          if f of a then a[f] else a[f] = []
        obj = (a, f) ->
          if f of a then a[f] else a[f] = {}
        inc = (a, f) ->
          if f of a then a[f]++ else
            a[f] = 1
            0

        Birthdate = constants.categoryName.participant.metricName.birthdate
        pseudoMetric =
          indicator:
            id: 'indicator'
            name: 'indicator'
            type: 'void'
            sort: 0
            assumed: true
          top:
            id: 'top'
            category: 'slot'
            name: 'type'
            type: 'top'
            sort: 1
            options:
              false: 'session'
              true: 'materials'
          name:
            id: 'name'
            #category: 'slot', 'asset'
            name: 'name'
            type: 'text'
            sort: 2
          date:
            id: 'date'
            category: 'slot'
            name: 'test date'
            type: 'date'
            description: 'Date on which this session was acquired'
            sort: 3
          release:
            id: 'release'
            category: 'slot'
            name: 'release'
            type: 'release'
            description: 'Level of data release to which depicted participant(s) consented'
            sort: 4
          age: # record
            id: 'age'
            category: Birthdate.category.id
            name: 'age'
            type: 'age'
            release: constants.release.EXCERPTS
            sort: Birthdate.id + 0.5
            description: 'Time between birthdate and test date'
            readonly: true
          summary:
            id: 'summary'
            category: 'slot'
            name: 'summary'
            type: 'text'
            sort: Infinity
            readonly: true
        constants.deepFreeze(pseudoMetric)
        getMetric = (m) ->
          pseudoMetric[m] || constants.metric[m]

        pseudoCategory =
          slot:
            id: 'slot'
            name: 'folder'
            not: 'No folders'
            template: ['top', 'name', 'date', 'release']
            fixed: true
          asset:
            id: 'asset'
            name: 'file'
            not: 'No files'
            template: ['name']
            description: 'Files included in this folder'
            fixed: true
        constants.deepFreeze(pseudoCategory)
        getCategory = (c) ->
          pseudoCategory[c] || constants.category[c]

        ###
        # We use the following types of data structures:
        #   Row = index of slot in slots and rows (i)
        #   Slot_id = Database id of container
        #   Record_id = Database id of record
        #   Category_id = Database id of record category (c)
        #   Count = index of record within category for slot (n)
        #   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
        ###

        TBody = $element[0].getElementsByTagName("tbody")[0]
        TFoot = $element[0].getElementsByTagName("tfoot")[0]

        Groups = []     # [] Array over categories :: {category: Category, metrics[]: Array of Metric}
        Cols = []       # [] Array over metrics :: {category: Category, metric: Metric} (flattened version of Groups)
        Rows = []       # [i] :: Row (-1 => foot)
        Cats = {}       # Set of used categories
        Order = []      # Permutation Array of Row in display order
        Expanded = undefined # Info
        SlotCount = 0


        class Row
          # Represents everything about a row.  Properties:
          #   i: Row index
          #   tr: DOM Element tr
          #   [Category_id]: (array of) metric data for category

          constructor: (i) ->
            @i = i ? Rows.length
            Rows[@i] = this
            @filt = true
            return

          add: (c, d) ->
            Cats[c] = true
            if v = this[c]
              if Array.isArray(v)
                n = v.push(d) - 1
              else
                this[c] = [v, d]
                n = 1
            else
              this[c] = d
              n = 0
            n

          list: (c) ->
            if v = this[c]
              if Array.isArray(v)
                v
              else
                [v]
            else
              []

          count: (c) ->
            if v = this[c]
              if Array.isArray(v)
                v.length
              else
                1
            else
              0

          get: (c, n) ->
            if v = this[c]
              if Array.isArray(v)
                v[n || 0]
              else if !n
                v

          set: (c, n, d) ->
            n ||= 0
            if n || Array.isArray(this[c])
              if d?
                this[c].splice(n, 1, d)
              else
                this[c].splice(n, 1)
            else
              this[c] = d
            return

          Object.defineProperty @prototype, 'key',
            get: -> @get(Key.id)


        class Info
          # Represents everything we know about a specific cell.  Properties:
          #   cell: target TD element
          #   id: cell.id
          #   i: Row
          #   n: Count (index of count), optional [0]
          #   m: index into Cols
          #   cols: Groups element
          #   col: Cols element
          #   category: Category
          #   c: Category_id
          #   count: Count[i][c]
          #   metric: Metric
          #   row: Rows[i]
          #   slot: Container
          #   d: Data
          #   record: Record
          #   asset: Asset
          #   v: Data value

          constructor: (x) ->
            if typeof x == 'number'
              @i = x
            else if x
              @cell = x
            @parseId()
            return

          parseId: (i) ->
            return unless (if i? then @id = i else i = @id) and (i = stripPrefix(i, ID+'-'))
            s = i.split '_'
            switch s[0]
              when 'add', 'more', 'no'
                @t = s[0]
                @i = parseInt(s[1], 10)
                @c = parseIntish(s[2])
              when 'metric'
                @t = s[0]
                @m = parseInt(s[1], 10)
              when 'category'
                @t = s[0]
                @c = parseIntish(s[1])
              else
                @i = parseInt(s[0], 10)
                @m = parseInt(s[1], 10)
                if 2 of s
                  @n = parseInt(s[2], 10)
            true

          properties =
            n: ->
              0
            id: ->
              @cell?.id
            cols: ->
              c = @c
              @cols = Groups.find (col) -> `col.category.id == c`
            col: ->
              Cols[@m]
            category: ->
              if (c = @col)?
                c.category
              else if this.hasOwnProperty('c')
                @category = getCategory(@c)
            c: ->
              @category?.id
            metric: ->
              @col?.metric
            row: ->
              Rows[@i]
            count: ->
              @row.count(@c)
            tr: ->
              @row.tr
            d: ->
              @row.get(@c, @n)
            p: ->
              cls = 'ss-'
              if typeof (c = @c) != 'number'
                cls += c.charAt(0)
              cls
            slot: ->
              @row.get('slot', if @c == 'slot' then @n)?.slot
            record: ->
              @d?.record
            asset: ->
              @d?.asset
            v: ->
              if (d = @d) && (m = @metric)
                d[m.id]

          property = (v, f) ->
            get: f
            set: (x) ->
              Object.defineProperty @, v,
                value: x
                writable: true
                configurable: true
                enumerable: true
              return

          for v, f of properties
            properties[v] = property(v, f)

          Object.defineProperties @prototype,
            properties

        parseId = (el) ->
          return unless el.tagName == 'TD'
          info = new Info(el)
          info if info.c?

        ################################# Populate data structures

        # Fill Cols and Groups from volume metrics
        populateCols = (slot) ->
          Cols = []
          all = Object.keys(volume.metrics).sort(byNumber).map((i) -> constants.category[i])
          if slot
            slot = undefined
            cats = all.filter((c) -> $scope.showGlobal || (Cats[c.id] ? Editing))
            cats.unshift(pseudoCategory.asset)
            cats.unshift(pseudoCategory.slot)
            $scope.anyGlobal = all.some((c) -> Cats[c.id] == false)
          else
            slot = [pseudoMetric.summary]
            cats = [Key, pseudoCategory.slot]
            $scope.anyGlobal = false

          $scope.groups = Groups = cats.map (category) ->
            metrics = (category.template || volume.metrics[category.id]).map(getMetric)
            if !Editing && Birthdate in metrics
              (slot || metrics).push(pseudoMetric.age)
            if slot && category.id == 'slot'
              metrics.push.apply metrics, slot
            metrics.sort(bySortId)
            si = Cols.length
            Cols.push.apply Cols, metrics.map (m) ->
              category: category
              metric: m
            l = metrics.length
            Cols[si].first = Cols[si+l-1].last = l
            {
              category: category
              metrics: metrics
              start: si
            }
          $scope.cols = Cols
          $scope.views = all
          partindex = false
          i = 0
          while i < $scope.views.length
            if $scope.views[i].name == 'participant'
              partindex = true
            i++
          if !Editing or partindex == false
            $scope.views.unshift pseudoCategory.slot
          else
            $scope.views.splice 1, 0, pseudoCategory.slot
          return

        populateSlotData = (s) ->
          d =
            slot: s
            id: s.id
            top: s.top || false
            name: s.name
            date: s.date
            release: s.release ? -1
          if s.top == 'global'
            d.global = true
            d.summary = constants.message('global.name') + " (only displayed in volume description)"
          d

        populateRecordData = (r) ->
          d = Object.create(r.measures)
          d.record = r
          d.id = r.id
          d

        populateAssetData = (a) ->
          d =
            asset: a
            id: a.id
            name: a.name
          d

        # Fill all Data values for Row i
        populateSlot = (slot) ->
          row = new Row()
          row.add('slot', populateSlotData(slot))

          for rr in slot.records when (record = rr.record)
            d = populateRecordData(record)
            if 'age' of rr
              d.age = rr.age
            row.add(record.category, d)

          for assetId, asset of slot.assets
            row.add('asset', populateAssetData(asset))

          row

        populateSlots = ->
          SlotCount = -1
          for ci, slot of volume.containers
            SlotCount++
            if slot.top == 'global'
              for rr in slot.records when (record = rr.record)
                record.global = true
                Cats[record.category] ||= false
              continue
            populateSlot(slot)

        populateRecord = (record) ->
          row = new Row()
          row.add(record.category, populateRecordData(record))
          row

        populateRecords = ->
          records = {}
          nog = undefined
          for r, record of volume.records when record.category == Key.id
            if nog then nog++ else nog = 1
            delete record.global
            records[r] = populateRecord(record)

          SlotCount = -1
          nor = undefined
          for ci, slot of volume.containers
            SlotCount++
            recs = slot.records
            any = false
            for rr in recs when (row = records[rr.id])
              d = populateSlotData(slot)
              if 'age' of rr
                d.age = rr.age
              if d.global
                unless rr.record.global
                  rr.record.global = true
                  nog--
                row.partial = true
              else
                d.summary = (rrr.record.displayName for rrr in recs when rrr.id != rr.id).join(', ')
                row.partial ||= !Segment.isFull(rr.segment)
              row.add('slot', d)
              any = true
            unless any || slot.top == 'global'
              nor ||= new Row()
              d = populateSlotData(slot)
              d.summary = (rrr.record.displayName for rrr in recs).join(', ')
              nor.add('slot', d)
          if nog == 0 && nor
            delete Rows[nor.i]

        ################################# Generate HTML

        # Add or replace the text contents of cell c for measure/type m with value v
        generateText = (info) ->
          $(cell = info.cell).empty()
          cell.className = ''
          v = info.v
          slot = info.slot
          if info.col.first && info.d
            if info.c == 'asset'
              cell.classList.add('clickable')
              cell.onclick = (event) ->
                $location.url(if Editing then slot.editRoute(t) else slot.route(t))
              a = cell.appendChild(document.createElement('a'))
              icon = a.appendChild(document.createElement('img'))
              asset = info.asset
              icon.src = asset.icon
              icon.alt = "format-" + asset.format.extension + "-icon"
              icon.className = "format hint-format-" + asset.format.extension
              t = {asset:asset.id}
              a.setAttribute('href', if Editing then slot.editRoute(t) else slot.route(t))
              icon = cell.appendChild(document.createElement('span'))
              icon.className = 'icon release ' + constants.release[asset.release] + ' hint-release-' + constants.release[asset.release]
              icon.setAttribute("role", "img")
              icon.setAttribute("aria-label", constants.release[asset.release] + " release level") 
            else
              if Editing && Key.id == info.c
                cell.classList.add('folder-type') # XXX not necessarily!?
                cell.classList.add('clickable')
                del = cell.appendChild(document.createElement('a'))
                del.className = 'clickable trash icon'
                $(del).on 'click', $scope.$lift(clickRemove)
              if info.c == 'slot'
                a = cell.appendChild(document.createElement('a'))
                a.className = "session icon hint-action-slot"
                a.setAttribute('href', if Editing then slot.editRoute() else slot.route())
                span = a.appendChild(document.createElement('span'))
                span.className = "accessible-hidden"
                span.innerHTML = "open this folder"
          switch info.metric.id
            when 'name'
              v ?= ''
            when 'release'
              if v >= 0 || !slot?.top
                cn = constants.release[v]
                cell.className = cn + ' release icon hint-release-' + cn
              cell.classList.add('null') if slot?.top
              v = ''
            when 'date'
              cell.classList.add('null') if slot?.top
            when 'age'
              v = display.formatAge(v)
            when 'top'
              if v?
                v = info.metric.options[v]
              else if Editing && info.col.first
                cell.classList.add('add')
                cell.classList.add('button')
                v = 'add folder'
            when 'summary'
              if info.d.global && Editing
                cell.classList.add('spreadsheet-global-record-cell')
                del = cell.appendChild(document.createElement('a'))
                del.className = 'button mini global-record white icon-text'
                delicon = del.appendChild(document.createElement('span'))
                delicon.className = 'icon trash'
                del.appendChild(document.createTextNode('Remove'))
                $(del).on 'click', $scope.$lift(clickGlobal)
            else
              if info.metric.type == 'void' && info.d
                cell.className = 'clickable' if Editing && Key.id != info.c
                v = info.metric.name
          if info.metric.long
            cell.classList.add('long')
          if v?
            cell.classList.remove('blank')
          else
            cell.classList.add('blank')
            v = info.metric.assumed || ''
          cell.appendChild(document.createTextNode(v))
          cell.id = info.id
          if info.d
            cell.classList.add(cls = info.p + info.d.id)
            cell.classList.add(cls + '_' + info.metric.id)
          return

        generateAdd = (info, td) ->
          info.m = info.cols.start
          width = info.width || info.cols.metrics.length
          if info.metric.type == 'top' || typeof info.metric.id == 'number' && info.metric.type != 'void'
            info.id = ID+'-'+info.i+'_'+info.cols.start+(if info.hasOwnProperty('n') then '_'+info.n else '')
            info.cell = info.tr.appendChild(document.createElement('td'))
            generateText(info)
            info.tr.insertBefore(info.cell, td)
            if width > 1
              td.setAttribute("colspan", width-1)
              td.classList.add('prompt')
              td.appendChild(document.createTextNode("\u2190 add " + info.category.name)) if info.c != 'slot'
            else
              info.tr.removeChild(td)
          else
            td.setAttribute("colspan", width)
            td.classList.add('add')
            td.classList.add('clickable')
            td.id = ID + '-add_' + info.i + '_' + info.c
            td.appendChild(document.createTextNode(if Editing && info.metric.type != 'void' then "add " + info.category.name else info.category.not))

        generateMultiple = (info) ->
          t = info.count
          return if (if info.hasOwnProperty('n') then info.n < t else t == 1)
          td = info.tr.appendChild(document.createElement('td'))
          width = info.cols.metrics.length
          td.setAttribute("colspan", width)
          if info.hasOwnProperty('n') || t <= 1
            td.className = 'null'
            if !info.n || info.n == t
              if Editing && info.c != 'slot' && info.c != Key.id
                generateAdd(info, td)
              else if !info.n
                td.appendChild(document.createTextNode(info.category.not))
                if Editing && info.c == 'slot' && Key.id != 'slot' && Key.id != 1
                  td.classList.add('spreadsheet-global-record-cell')
                  add = td.appendChild(document.createElement('a'))
                  add.className = 'button mini white global-record icon-text'
                  addicon = add.appendChild(document.createElement('span'))
                  addicon.className = 'add icon'
                  add.appendChild(document.createTextNode('Apply to whole volume'))
                  $(add).on 'click', $scope.$lift(clickGlobal)
                td.id = ID + '-no_' + info.i + '_' + info.c
          else
            td.className = 'more'
            td.id = ID + '-more_' + info.i + '_' + info.c
            if Editing && info.c == 'slot' && Key.id != 'slot' && t == SlotCount && !info.row.partial && info.row.key
              td.classList.add('spreadsheet-global-record-cell')
              add = td.appendChild(document.createElement('a'))
              add.className = 'button mini yellow global-record icon-text hint-action-globalrecord'
              add.appendChild(document.createTextNode('Convert to ' + constants.message('global.name').toLowerCase() + ' (irreversible)'))
              $(add).on 'click', $scope.$lift(clickGlobal)
            td.appendChild(document.createTextNode(t + " " + info.category.name + "s"))
          td

        # Add all the measure tds to row i for count n, record r
        generateRecord = (info) ->
          return unless l = info.cols.metrics.length # impossible?
          if td = generateMultiple(info)
            unless info.hasOwnProperty('n')
              cls = info.p
              for n in [0..info.count-1] by 1
                td.classList.add(cls + info.row.get(info.c, n).id)
            return
          pre = ID + '-' + info.i + '_'
          post = if info.hasOwnProperty('n') then '_' + info.n else ''
          generateCell = (i) ->
            info.m = info.cols.start+i
            info.id = pre + info.m + post
            info.cell = info.tr.appendChild(document.createElement('td'))
            generateText(info)
            return
          if info.category.id == 'slot' && info.d.global
            generateCell(l-1) # summary
            info.cell.setAttribute("colspan", l)
            return
          for mi in [0..l-1] by 1
            generateCell(mi)
          return

        # Fill out Rows[i].
        generateRow = (i) ->
          info = new Info(i)
          row = info.row
          if row.tr
            $(row.tr).empty()
            row.tr
          else
            row.tr = document.createElement('tr')
          tr = row.tr
          tr.id = ID + '_' + i
          tr.data = i

          for col in Groups
            info.category = (info.cols = col).category
            generateRecord(info)
          return

        generateFoot = ->
          if row = Rows[-1]
            $(Rows[-1].tr).empty()
          else
            row = new Row(-1)
            row.tr = TFoot.appendChild(document.createElement('tr'))
            row.tr.id = ID + '_add'
          info = new Info(row.i)
          info.cols = Groups[0]
          info.category = Key
          td = info.tr.appendChild(document.createElement('td'))
          td.setAttribute("colspan", info.cols.metrics.length)
          td.className = 'null'
          info.width = Cols.length
          generateAdd(info, td)

        # Update all age displays.
        $scope.$on 'displayService-toggleAge', ->
          info = new Info()
          for m, mi in Cols
            continue unless m.metric.id == 'age'
            info.m = mi
            pre = ID + '-'
            mid = '_' + mi
            for i, n in Order
              info.i = i
              if info.v
                info.cell = document.getElementById(pre + i + mid)
                generateText(info) if info.cell
            if Expanded?.c == info.c && Expanded.count > 1
              info.i = Expanded.i
              premid = pre + info.i + mid + '_'
              for n in [0..Expanded.count-1] by 1
                info.n = n
                if info.v
                  info.cell = document.getElementById(premid + n)
                  generateText(info) if info.cell

        ################################# Place DOM elements

        # Place all rows into spreadsheet.
        fill = ->
          collapse()
          n = 0
          next = TBody.firstChild
          for i in Order
            tr = Rows[i].tr
            if Rows[i].filt && n++ < Limit
              if next == tr
                next = next.nextSibling
              else if next
                TBody.insertBefore(tr, next)
              else
                TBody.appendChild(tr)
            else
              if next == tr
                next = next.nextSibling
                TBody.removeChild(tr)
              else if tr.parentNode
                TBody.removeChild(tr)
          if n > Limit
            $scope.more = n
          else
            delete $scope.more
          return

        # Populate order based on compare function applied to values.
        sort = (get) ->
          idx = new Array(Order.length)
          for o, i in Order
            idx[o] = i
          Order.sort (i, j) ->
            byMagic(get(i), get(j)) || idx[i] - idx[j]
          return

        currentSort = undefined
        currentSortDirection = false

        # Sort by column
        sortBy = (col, dir) ->
          if currentSort == col && !dir?
            currentSortDirection = !currentSortDirection
            Order.reverse()
          else
            unless currentSort
              Order.sort (i, j) -> byNumber(Rows[i].key?.id, Rows[j].key?.id)
            c = col.category.id
            m = col.metric.id
            sort (i) ->
              Rows[i].get(c)?[m]
            currentSort = col
            if dir
              currentSortDirection = true
              Order.reverse()
            else
              currentSortDirection = false
          return

        $scope.colClasses = (col) ->
          cls = []
          if typeof col == 'object'
            cls.push 'first' if col.first
            cls.push 'last' if col.last
          cls.push 'sort'
          if currentSort == col
            cls.push 'intransitive sort-'+(if currentSortDirection then 'desc' else 'asc')
          else
            cls.push 'intransitive sortable'
          cls

        ################################# Backend saving

        setFocus = undefined

        saveRun = (cell, run) ->
          messages.clear(cell)
          cell.classList.remove('error')
          cell.classList.add('saving')
          run.then (res) ->
              cell.classList.remove('saving')
              $scope.form.$setPristine()
              res
            , (res) ->
              cell.classList.remove('saving')
              cell.classList.add('error')
              messages.addError
                body: 'Could not save data' # FIXME
                report: res
                owner: cell
              return

        addRow = (row) ->
          Order.push(row.i)
          generateRow(row.i)
          TBody.appendChild(row.tr)
          row

        createSlot = (info) ->
          saveRun info.cell, volume.createContainer().then (slot) ->
            arr(slot, 'records')
            addRow(populateSlot(slot))

        createRecord = (info) ->
          saveRun info.cell, volume.createRecord(info.c || undefined).then (record) ->
            addRow(populateRecord(record))

        createNew = (info) ->
          if info.c == 'slot'
            createSlot(info)
          else if typeof info.c == 'number'
            createRecord(info)

        removeRow = (i) ->
          unedit(false)
          row = Rows[i]
          delete Rows[i]
          TBody.removeChild(row.tr) if row.tr.parentNode
          Order.remove(i)
          Expanded = undefined if Expanded?.i == i
          return

        removeSlot = (info) ->
          # assuming we have a container
          saveRun info.cell, info.slot.remove().then (done) ->
            unless done
              messages.add
                body: "You must remove all associated records and files before you can remove this folder."
                type: 'red'
                owner: info.cell
              return
            removeRow(info.i)
            return

        removeRecord = (info) ->
          saveRun info.cell, info.record.remove().then (done) ->
            unless done
              messages.add
                body: "You must remove all associated sessions and materials before you can remove this " + info.category.name + "."
                type: 'red'
                owner: info.cell
              return
            removeRow(info.i)
            return

        setRecord = (info, record) ->
          add = ->
            if record
              info.slot.addRecord(record)
            else if record != null
              info.slot.newRecord(info.c || '')
          act =
            if info.record
              info.slot.removeRecord(info.record).then(add)
            else
              add()

          saveRun info.cell, act.then (rr) ->
            record = rr?.record
            o = info.d
            if record
              r = populateRecordData(record)
              if o
                info.row.set(info.c, info.n, r)
              else
                info.n = info.row.add(info.c, r)
            else
              info.row.set(info.c, info.n, undefined)

            collapse()
            generateRow(info.i)
            expand(info) if info.n
            if record && setFocus == (i = info.id) && (i = document.getElementById(i)?.nextSibling) && (i = parseId(i))
              select(i)
            setFocus = undefined
            record

        updateDatum = (info, v) ->
          info.v = v
          info.d[info.metric.id] = v
          if info.metric.id == 'top'
            collapse()
            generateRow(info.i)
            expand(info)
          else if info.category.fixed
            generateText(info)
          else
            for li in TBody.getElementsByClassName(info.p + info.d.id + '_' + info.metric.id)
              info.cell = li
              generateText(info)
          return

        saveDatum = (info, v) ->
          if info.c == 'slot'
            data = {}
            v = v == 'true' if info.metric.id == 'top'
            data[info.metric.id] = v ? ''
            return if `info.slot[info.metric.id] == v`
            saveRun info.cell, info.slot.save(data).then ->
              updateDatum(info, v)
              return
          else if info.c == 'asset'
            data = {}
            t = info.metric.id
            data[t] = v ? ''
            return if info.asset[t] == data[t]
            saveRun info.cell, info.asset.save(data).then ->
              updateDatum(info, v)
              return
          else
            return if info.record.measures[info.metric.id] == v
            saveRun info.cell, info.record.measureSet(info.metric.id, v).then ->
              updateDatum(info, v)
              return

        ################################# Interaction

        # Collapse any expanded row.
        collapse = ->
          return unless Expanded
          i = Expanded.i
          tr = Expanded.tr
          Expanded = undefined
          tr.classList.remove('expand')
          t = 0
          while (el = tr.nextSibling) && el.data == i
            t++
            $(el).remove()

          el = tr.firstChild
          while el
            el.removeAttribute("rowspan")
            el = el.nextSibling

          t

        # Expand (or collapse) a row
        expand = (info) ->
          if Expanded?.i == info.i && `Expanded.c == info.c`
            if info.t == 'more'
              collapse()
            return
          collapse()

          return if info.i < 0
          Expanded = new Info(info.i)
          Expanded.c = info.c
          info.tr.classList.add('expand')

          max = Expanded.count
          max++ if Editing && Expanded.c != Key.id && Expanded.c != 'slot'
          return if max <= 1
          next = info.tr.nextSibling
          start = Expanded.count == 1
          for n in [+start..max-1] by 1
            Expanded.n = n
            Expanded.tr = TBody.insertBefore(document.createElement('tr'), next)
            Expanded.tr.data = Expanded.i
            Expanded.tr.className = 'expand'
            generateRecord(Expanded)
          Expanded.tr = info.tr

          max++ unless start
          el = info.tr.firstChild
          while el
            info = new Info(el)
            if `info.c != Expanded.c`
              el.setAttribute("rowspan", max)
            el = el.nextSibling
          return

        save = (info, type, value) ->
          if value == ''
            value = undefined
          else switch type
            when 'record'
              if value == 'new'
                setRecord(info)
              else if value == 'remove'
                setRecord(info, null) if info.d
              else if v = stripPrefix(value, 'add_')
                u = v.indexOf('_')
                info.metric = constants.metric[v.slice(0,u)]
                v = v.slice(u+1)
                setRecord(info).then (r) ->
                  info.record = r
                  saveDatum(info, v) if r
                  return
              else if !isNaN(v = parseInt(value, 10))
                if v != info.d?.id
                  setRecord(info, volume.records[v])
              return
            when 'options'
              # force completion of the first match
              # this completely prevents people from using prefixes of options but maybe that's reasonable
              c = optionCompletions(value) if value
              value = c[0] if c?.length

          if type == 'ident'
            r = editScope.identCompleter(value)
            r.find((o) -> o.default)?.run(info) if Array.isArray(r)
          else if info.i == -1
            return unless value
            createNew(info).then (row) ->
              saveDatum(new Info(row.tr.firstChild), value)
              return
          else
            saveDatum(info, value)
          return

        editScope = $scope.$new(true)
        editScope.constants = constants
        editInput = editScope.input = {}
        editCellTemplate = $compile($templateCache.get('volume/spreadsheetEditCell.html'))
        editCell = undefined

        unedit = (event) ->
          return unless edit = editCell
          editCell = undefined
          cell = edit.parentNode
          $(edit).remove()
          return unless cell?.parentNode
          cell.classList.remove('editing')
          tooltips.clear()

          info = new Info(cell)
          save(info, editScope.type, editInput.value) if event != false
          info

        recordDescription = (r) ->
          k = Object.keys(r.measures)
          if k.length
            k.sort(byNumber).map((m) -> r.measures[m]).join(', ')
          else
            '[' + r.id + ']'

        edit = (info) ->
          switch info.t
            when undefined
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute({asset:info.d.id}))
                return
              m = info.metric
              return if m.readonly
              editScope.type = m.type
              mi = m.id
              editScope.options = m.options
              if m.type == 'void'
                editScope.type = 'record'
                editScope.options =
                  remove: info.category.not
                editScope.options[v = info.d.id] = m.name
              else if info.c == 'slot'
                return if info.slot?.top && (mi == 'date' || mi == 'release')
                v = info.slot?[mi]
                v = !!v if mi == 'top' && info.slot
                v = (v ? -1)+'' if mi == 'release'
              else if info.c == 'asset' # not reached
                v = info.asset[mi]
              else
                v = info.record?.measures[mi]
                if info.col.first && info.category != Key
                  editScope.type = 'ident'
                  editScope.info = info
                  rs = []
                  mf = (r) -> (m) -> r.measures[m]
                  for ri, r of volume.records
                    if r.category == info.c && !info.row.list(info.c).some((d) -> `d.id == ri`) && !r.global
                      rs.push
                        r:r
                        v:(r.measures[mi] ? '').toLowerCase()
                        d:recordDescription(r)
                  editScope.records = rs.sort((a, b) -> byMagic(a.v, b.v))
                else if m.options
                  editScope.type = 'options'
                else if m.long
                  editScope.type = 'long'
              editInput.value = (v ? '')+''
            when 'add'
              if info.i == -1
                createNew(info)
                return
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute() + '?file=open')
                return
              c = info.category
              editInput.value = (info.d?.id ? 'remove')+''
              editScope.type = 'record'
              editScope.options =
                new: 'Create new ' + c.name
                remove: c.not
              for ri, r of volume.records
                if r.category == c.id && (!info.row.list(info.c).some((d) -> `d.id == ri`) || `ri == editInput.value`) && !r.global
                  editScope.options[ri] = r.displayName
              ms = info.cols.metrics
              # detect special cases: singleton or unitary records
              if ms.length == 1
                if ms[0].type == 'void' && Object.keys(editScope.options).length > 2
                  # singleton: id only, existing record(s)
                  delete editScope.options['new']
                else if ms[0].options
                  # unitary: single metric with options
                  delete editScope.options['new']
                  for o in ms[0].options
                    found = false
                    for ri, r of volume.records
                      if r.category == c.id && r.measures[m.id] == o
                        found = true
                        break
                    editScope.options['add_'+ms[0].id+'_'+o] = o unless found
            else
              return

          e = editCellTemplate editScope, (e) ->
            info.cell.insertBefore(editCell = e[0], info.cell.firstChild)
            info.cell.classList.add('editing')
            return
          e.on 'click', ($event) ->
            # prevent other ng-click handlers from taking over
            $event.stopPropagation()
            return

          tooltips.clear()
          $timeout ->
            input = e.find('[name=edit]')
            input.filter('input,textarea').focus().select()
            input.filter('select').focus().one('change', $scope.$lift(editScope.unedit))
            return
          return

        unselect = ->
          styles.clear()
          unedit(false)
          return

        $scope.$on '$destroy', unselect

        select = (info) ->
          styles.clear()
          unedit()
          expand(info)
          if !info.t
            for c in info.cell.classList when c.startsWith('ss-')
              styles.set('.' + c + '{background-color:' +
                (if c.includes('_', 4) then 'rgba(243,180,140,0.7)' else 'rgba(243,180,140,0.4)') +
                ';\n text-}')
              info.cell.classList.add('selected')

          if Editing
            edit(info)
          else if info.category
            $scope.filter.add(info)
          return

        $scope.click = (event) ->
          return unless info = parseId(event.target)

          select(info)
          if info.metric?.id == 'age'
            display.toggleAge()
          return

        doneEdit = (event, info) ->
          if info && event && event.$key == 'Tab'
            setFocus = !event.shiftKey && info.cell.id
            c = info.cell
            while true
              c = if event.shiftKey then c.previousSibling else c.nextSibling
              return unless c && i = parseId(c)
              break unless i.category?.id == 'asset' || i.metric?.type == 'void' # skip "delete" actions
            select(i)

          return

        editScope.unedit = (event) ->
          doneEdit(event, unedit(event))
          event.preventDefault() if event
          false

        editSelect = (event) ->
          editInput.value = @text
          editScope.unedit(event)
          return # inputCompleter needs an undefined to do nothing

        editScope.identCompleter = (input) ->
          info = editScope.info
          o = []
          defd = false
          add = (t, f, d) ->
            o.push
              text: t
              select: (event) ->
                info = unedit(false)
                f(info)
                doneEdit(event, info)
                return
              run: f
              default: d && !defd
            defd ||= d
          if info.d
            if input == info.record.measures[info.metric.id]
              add("Keep " + info.record.displayName,
                -> return,
                true)
            if !input
              add("Remove " + info.record.displayName + " from this session",
                (info) -> setRecord(info, null),
                true)
          if !info.d || input && input != info.record.measures[info.metric.id]
            inputl = (input ? '').toLowerCase()
            set = (r) -> (info) ->
              setRecord(info, r)
            rs = (r for r in editScope.records when r.v.startsWith(inputl))
            for r in rs
              add("Use " + info.category.name + ' ' + r.d, set(r.r), input && rs.length == 1 || r.v == inputl)
            os = if info.metric.options
                (x for x in info.metric.options when x.toLowerCase().startsWith(inputl))
              else
                []
            if input && !os.length
              os = [input]
            os.forEach (i) ->
              if info.d
                add("Change all " + info.record.displayName + " " + info.metric.name + " to '" + i + "'",
                  (info) -> saveDatum(info, i),
                  input && !rs.length && os.length == 1 || i == input)
              add("Create new " + info.category.name + " with " + info.metric.name + " '" + i + "'",
                (info) -> setRecord(info).then((r) ->
                  info.record = r
                  saveDatum(info, i) if r
                  return),
                input && !rs.length && os.length == 1 || i == input)
          if o.length then o else input

        optionCompletions = (input) ->
          i = input.toLowerCase()
          (o for o in editScope.options when o.toLowerCase().startsWith(i))

        editScope.optionsCompleter = (input) ->
          match = optionCompletions(input)
          switch match.length
            when 0
              input
            when 1
              match[0]
            else
              ({text:o, select: editSelect, default: input && i==0} for o, i in match)

        $scope.clickHeader = (col) ->
          unless Editing
            $scope.filter.add(col)
          if col.metric
            sortBy(col)
            fill()
          return

        clickRemove = (event) ->
          return unless info = parseId(event.target.parentNode)
          if info.c == 'slot'
            removeSlot(info)
          else if typeof info.c == 'number'
            removeRecord(info)
          event.stopPropagation()
          false

        clickGlobal = (event) ->
          return unless info = parseId(event.target.parentNode)
          rec = info.row.key.record
          act =
            if rec.global
              volume.top.removeRecord(rec).then () ->
                delete rec.global
                info.row.set(info.c, info.n, undefined)
                return
            else if info.count
              router.http(router.controllers.deleteRecordAllSlot, rec.id).then () ->
                volume.top.addRecord(rec).then () ->
                  true
            else
              volume.top.addRecord(rec).then () ->
                rec.global = true
                info.row.set(info.c, info.n, populateSlotData(volume.top))
                return
          saveRun info.cell, act.then (r) ->
            if r
              $route.reload()
              return
            collapse()
            generateRow(info.i)
            expand(info) if info.n
            return
          return

        $scope.unlimit = ->
          Limit = Infinity
          fill()
          return

        $scope.showHideGlobal = ->
          $scope.showGlobal = !$scope.showGlobal
          $scope.setKey(Key.id)
          return

        $scope.tabOptionsClick = false
        $scope.tabOptionsToggle = ($event) ->
          $scope.tabOptionsClick = !$scope.tabOptionsClick
          $event.stopPropagation()
          false

        ################################# main

        pivotOpts = undefined
        $scope.pivot =
          show: ->
            @run(Rows)
            return
          hide: ->
            @clear()
            return
          load: (opts) ->
            if @run
              if opts
                @run(Rows, opts)
              else
                @clear()
            else
              pivotOpts = opts
            return
          init: ->
            @run(Rows, pivotOpts) if pivotOpts
            pivotOpts = undefined
            return

        $scope.filter =
          update: (f) ->
            unedit(false)
            setFilter(f)
            fill()
            return
          accept: (f) ->
            if f.category == pseudoCategory.slot
              if Key == pseudoCategory.slot
                f.metric
              else
                f.metric != pseudoMetric.summary
            else
              f.category != pseudoCategory.asset && (Key == pseudoCategory.slot || Key == f.category)
          add: (info) ->
            return unless @accept(info)
            last = @list.pop()
            if last?.op
              @list.push(last)
            @list.push
              category: info.category
              metric: if info.metric && info.metric.type != 'void' then info.metric else pseudoMetric.indicator
              value: if info.metric?.type == 'numeric' then parseFloat(info.v) else info.v
            return
          count: 0

        setFilter = (f) ->
          c = 0
          if !f
            for row in Rows when row
              row.filt = true
          else if Key.id == 'slot'
            for row in Rows when row
              s = row.slot
              c++ unless row.filt = f(s, s.slot.records)
          else
            for row in Rows when row
              c++ unless row.filt = f(row.key?.record, row.list('slot'))
          $scope.filter.count = c
          if $scope.pivot.active
            $scope.pivot.show()
          return

        # Call all populate functions
        setKey = (key) ->
          unselect()
          oldKey = Key
          Key = $scope.key = key? && volume.metrics[key] && getCategory(key) || pseudoCategory.slot
          foot = Rows[-1]
          Rows = []
          Cats = {}
          if bySlot = Key == pseudoCategory.slot
            populateSlots()
          else
            populateRecords()
          $scope.count = Rows.length
          $(TBody).empty()
          Expanded = undefined
          Rows[-1] = foot if foot
          populateCols(bySlot)
          if Key != oldKey || Order.length != Rows.length
            Order = (row.i for row in Rows when row)
            currentSort = undefined
            sortBy(Cols.find((c) -> c.category.id == 'slot' && c.metric.id == 'date'))
          Order.sort (a, b) ->
            a - b
          for i in Order
            generateRow(i)
          if Editing
            generateFoot()
          tooltips.clear()
          $scope.filter.key = Key.id
          $scope.filter.list = $scope.filter.list.filter($scope.filter.accept)
          setFilter($scope.filter.make?())
          $location.replace().search('key', undefined)
          $scope.tabOptionsClick = undefined
          return

        $scope.setKey = (key) ->
          setKey(key)
          fill()
          sessionStorage.setItem 'spreadsheetFolder', key
          return

        $scope.state =
          get: ->
            state =
              key: Key.id
              sort: currentSort && {
                  c: currentSort.category.id
                  m: currentSort.metric.id
                  dir: currentSortDirection
                }
              filter: $scope.filter.list.map (f) ->
                c: f.category.id
                m: f.metric.id
                op: f.op
                v: f.value
              pivot: $scope.pivot.get?()
              public: @public
            delete state.sort unless state.sort
            l = state.filter.pop()
            if l?.op
              state.filter.push(l)
            delete state.pivot unless state.pivot
            delete state.public unless state.public
            state

          put: (state, key) ->
            if state.filter
              l = []
              for f in state.filter
                r =
                  category: getCategory(f.c)
                  metric: getMetric(f.m)
                  op: f.op
                  value: f.v
                l.push r if r.category && r.metric
              $scope.filter.list = l
            else
              $scope.filter.list = [
                category: pseudoCategory.slot
                metric: pseudoMetric.top
              ]
            setKey(key || state.key)
            if state.sort && col = Cols.find((c) -> c.category.id == state.sort.c && c.metric.id == state.sort.m)
              sortBy(col, state.sort.dir)
            fill()
            $scope.pivot.load(state.pivot)
            return

          name: 'default'
          save: ->
            return unless name = @name
            state = @get()
            router.http(router.controllers.postVolumeState, volume.id, encodeURIComponent(name), state).then ->
                volume.state[name] = state
                messages.add
                  type: 'green'
                  body: 'Display mode "' + name + '" saved successfully.'
                return
              , (res) ->
                messages.addError
                  body: 'Error saving report state'
                  report: res
                return
            return

          delete: ->
            return unless @name
            router.http(router.controllers.deleteVolumeState, volume.id, encodeURIComponent(@name)).then =>
                delete volume.state[@name]
                @name = undefined
                return
              , (res) ->
                messages.addError
                  body: 'Error deleting report state'
                  report: res
                return
            return

          restore: (key, state) ->
            state = state || volume.state?[@name] || {}
            state.public = !!state.public
            @put(state, key)
            return

        if volume.state
          volume.state['NIH Inclusion Enrollment Report'] ?=
            key: constants.categoryName.participant.id
            pivot:
              cols: ["participant ethnicity", "participant gender"]
              rows: ["participant race"]
              rendererName: "Table"
              aggregatorName: "Count"
            filter: [
              {c: "slot", m: "top", op: "false"},
              {c: "slot", m: "date", op: "ge"},
              {c: "slot", m: "date", op: "le", v: $filter('date')(new Date(), 'yyyy-MM-dd')}]
          volume.state['Session release level summary'] ?=
            key: 'slot'
            pivot:
              cols: []
              rows: ["release"]
              rendererName: "Table"
              aggregatorName: "Count"
            filter: [
              {c: "slot", m:"top", op:"false"}]

        $scope.zip = (event) ->
          event.preventDefault()
          cs = []
          for row in Rows when row
            cs.push
              i: row.slot.id
              f: !!row.filt
          cs.sort (a, b) -> a.i-b.i
          f = $scope.filter.count < cs.length/2
          l = []
          b = undefined
          add = ->
            if b
              l.push(b.join('-'))
              b = undefined
          for c in cs
            if c.f == f
              add()
            else if b
              b[1] = c.i
            else
              b = [c.i]
          add()
          params = {}
          if l.length
            params[if f then 'exclude' else 'include'] = l.join(',')
          $location.url(router.volumeDescription([volume.id], params))
          return

        return
    ]
    link: ($scope, $element, $attrs) ->
      state = storage.getValue('spreadsheet-state')
      state = undefined unless state?.volume == $scope.volume.id
      $scope.state.restore($attrs.key || $location.search().key, state)
      # show the first tab on angular load or show the tab in the sessionSotrage
      folderTab = sessionStorage.getItem('spreadsheetFolder')
      if folderTab
        $scope.setKey(folderTab)
      else
        $scope.setKey($scope.views[0].id)
      $scope.$on 'refreshParent', ->
        $scope.volume.get(records: true).then ((res) ->
          $scope.volume.records = res.records
          if folderTab
            $scope.setKey(folderTab)
          else
            $scope.setKey($scope.views[0].id)
          return
        ), (res) ->
          messages.addError
            body: constants.message('Please refresh the page.')
            report: res
        return
      $scope.$on 'skiptrue', ->
        $scope.skiptrue = true
        return
      $scope.$on 'skipfalse', ->
        $scope.skiptrue = false
        return
      $scope.$on '$destroy', ->
        state = $scope.state.get()
        state.volume = $scope.volume.id
        storage.setValue('spreadsheet-state', state)
        return
      return
]
