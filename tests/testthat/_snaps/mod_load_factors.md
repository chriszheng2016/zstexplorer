# load_factors_server - reactives and output updates

    {"x":{"filter":"none","container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>factor_code<\/th>\n      <th>factor_name<\/th>\n      <th>factor_type<\/th>\n      <th>factor_group<\/th>\n      <th>factor_description<\/th>\n      <th>factor_lag_month<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-center"},{"className":"dt-right","targets":6},{"orderable":false,"targets":0}],"pageLength":10,"order":[],"autoWidth":false,"orderClasses":false,"serverSide":true,"processing":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[],"deps":[{"name":"dt-core","version":"1.10.20","src":{"href":"dt-core-1.10.20"},"meta":null,"script":"js/jquery.dataTables.min.js","stylesheet":["css/jquery.dataTables.min.css","css/jquery.dataTables.extra.css"],"head":null,"attachment":null,"package":null,"all_files":false},{"name":"jquery","version":"1.11.3","src":{"href":"jquery-1.11.3"},"meta":null,"script":"jquery.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"crosstalk","version":"1.1.0.1","src":{"href":"crosstalk-1.1.0.1"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.css","head":null,"attachment":null,"all_files":true}]} 

---

    {"x":{"filter":"top","filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"date\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"662601600000\" data-max=\"1585612800000\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"disabled\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"-64.792298\" data-max=\"3875.224783\" data-scale=\"15\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"-17.734978\" data-max=\"3529.467104\" data-scale=\"6\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Scroller"],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>date<\/th>\n      <th>period<\/th>\n      <th>stkcd<\/th>\n      <th>indcd<\/th>\n      <th>QR<\/th>\n      <th>CR<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"targets":5,"render":"function(data, type, row, meta) { return DTWidget.formatRound(data, 2, 3, \",\", \".\"); }"},{"targets":6,"render":"function(data, type, row, meta) { return DTWidget.formatRound(data, 2, 3, \",\", \".\"); }"},{"className":"dt-center"},{"className":"dt-right","targets":[5,6]},{"orderable":false,"targets":0}],"pageLength":10,"dom":"t","deferRender":true,"scrollY":320,"scrollX":true,"scroller":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"serverSide":true,"processing":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":["options.columnDefs.0.render","options.columnDefs.1.render"],"jsHooks":[],"deps":[{"name":"dt-core","version":"1.10.20","src":{"href":"dt-core-1.10.20"},"meta":null,"script":"js/jquery.dataTables.min.js","stylesheet":["css/jquery.dataTables.min.css","css/jquery.dataTables.extra.css"],"head":null,"attachment":null,"package":null,"all_files":false},{"name":"dt-ext-scroller","version":"1.10.20","src":{"href":"dt-ext-scroller-1.10.20"},"meta":null,"script":"js/dataTables.scroller.min.js","stylesheet":"css/scroller.dataTables.min.css","head":null,"attachment":null,"package":null,"all_files":false},{"name":"nouislider","version":"7.0.10","src":{"href":"nouislider-7.0.10"},"meta":null,"script":"jquery.nouislider.min.js","stylesheet":"jquery.nouislider.min.css","head":null,"attachment":null,"package":null,"all_files":true},{"name":"selectize","version":"0.12.0","src":{"href":"selectize-0.12.0"},"meta":null,"script":"selectize.min.js","stylesheet":"selectize.bootstrap3.css","head":null,"attachment":null,"package":null,"all_files":true},{"name":"jquery","version":"1.11.3","src":{"href":"jquery-1.11.3"},"meta":null,"script":"jquery.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"crosstalk","version":"1.1.0.1","src":{"href":"crosstalk-1.1.0.1"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.css","head":null,"attachment":null,"all_files":true}]} 

# load_factors_app - Module App works

    WAoAAAACAAMGAwACAwAAAAITAAAAAwAAAhMAAAAKAAAAEAAAAAEABAAJAAAADkZpbmFuY2lh
    bCBSaXNrAAAA/gAAAhMAAAAAAAAEAgAAAAEABAAJAAAABW5hbWVzAAAAEAAAAAAAAAD+AAAA
    DQAAAAwAAAABAAAAAgAAAAMAAAAEAAAABQAAAAYAAAAHAAAACAAAAAkAAAAKAAAACwAAAAwA
    AAANAAAACgAAAAEAAAACAAAAAwAAAAQAAAAFAAAABgAAAAcAAAAIAAAACQAAAAoAAAD+AAAA
    EAAAAAEABAAJAAAAAAAAAP4AAAMNAAAAAQAAAAEAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQ
    AAAAAgAEAAkAAAAHaW50ZWdlcgAEAAkAAAAWc2hpbnlBY3Rpb25CdXR0b25WYWx1ZQAAAP4A
    AAD+AAAEAgAAAf8AAAAQAAAACgAEAAkAAAAhbG9hZF9mYWN0b3JzX21vZHVsZS1mYWN0b3Jf
    Z3JvdXBzAAQACQAAACRsb2FkX2ZhY3RvcnNfbW9kdWxlLWZhY3RvcnNfaW5fZ3JvdXAABAAJ
    AAAAM2xvYWRfZmFjdG9yc19tb2R1bGUtZmFjdG9yc19pbmZvX3RhYmxlX2NlbGxfY2xpY2tl
    ZAAEAAkAAAAvbG9hZF9mYWN0b3JzX21vZHVsZS1mYWN0b3JzX2luZm9fdGFibGVfcm93c19h
    bGwABAAJAAAAM2xvYWRfZmFjdG9yc19tb2R1bGUtZmFjdG9yc19pbmZvX3RhYmxlX3Jvd3Nf
    Y3VycmVudAAEAAkAAAA0bG9hZF9mYWN0b3JzX21vZHVsZS1mYWN0b3JzX2luZm9fdGFibGVf
    cm93c19zZWxlY3RlZAAEAAkAAAAtbG9hZF9mYWN0b3JzX21vZHVsZS1mYWN0b3JzX2luZm9f
    dGFibGVfc2VhcmNoAAQACQAAACxsb2FkX2ZhY3RvcnNfbW9kdWxlLWZhY3RvcnNfaW5mb190
    YWJsZV9zdGF0ZQAEAAkAAAAgbG9hZF9mYWN0b3JzX21vZHVsZS1sb2FkX2ZhY3RvcnMABAAJ
    AAAAImxvYWRfZmFjdG9yc19tb2R1bGUtc2VsZWN0X2ZhY3RvcnMAAAD+AAACEwAAAAEAAAMQ
    AAAAAQAEAAkAAAZAeyJ4Ijp7ImZpbHRlciI6Im5vbmUiLCJjb250YWluZXIiOiI8dGFibGUg
    Y2xhc3M9XCJkaXNwbGF5XCI+XG4gIDx0aGVhZD5cbiAgICA8dHI+XG4gICAgICA8dGg+IDxc
    L3RoPlxuICAgICAgPHRoPmZhY3Rvcl9jb2RlPFwvdGg+XG4gICAgICA8dGg+ZmFjdG9yX25h
    bWU8XC90aD5cbiAgICAgIDx0aD5mYWN0b3JfdHlwZTxcL3RoPlxuICAgICAgPHRoPmZhY3Rv
    cl9ncm91cDxcL3RoPlxuICAgICAgPHRoPmZhY3Rvcl9kZXNjcmlwdGlvbjxcL3RoPlxuICAg
    ICAgPHRoPmZhY3Rvcl9sYWdfbW9udGg8XC90aD5cbiAgICA8XC90cj5cbiAgPFwvdGhlYWQ+
    XG48XC90YWJsZT4iLCJvcHRpb25zIjp7ImNvbHVtbkRlZnMiOlt7ImNsYXNzTmFtZSI6ImR0
    LWNlbnRlciJ9LHsiY2xhc3NOYW1lIjoiZHQtcmlnaHQiLCJ0YXJnZXRzIjo2fSx7Im9yZGVy
    YWJsZSI6ZmFsc2UsInRhcmdldHMiOjB9XSwicGFnZUxlbmd0aCI6MTAsIm9yZGVyIjpbXSwi
    YXV0b1dpZHRoIjpmYWxzZSwib3JkZXJDbGFzc2VzIjpmYWxzZSwiYWpheCI6eyJ0eXBlIjoi
    UE9TVCIsImRhdGEiOiJmdW5jdGlvbihkKSB7XG5kLnNlYXJjaC5jYXNlSW5zZW5zaXRpdmUg
    PSB0cnVlO1xuZC5zZWFyY2guc21hcnQgPSB0cnVlO1xuZC5lc2NhcGUgPSB0cnVlO1xudmFy
    IGVuY29kZUFtcCA9IGZ1bmN0aW9uKHgpIHsgeC52YWx1ZSA9IHgudmFsdWUucmVwbGFjZSgv
    Ji9nLCBcIiUyNlwiKTsgfVxuZW5jb2RlQW1wKGQuc2VhcmNoKTtcbiQuZWFjaChkLmNvbHVt
    bnMsIGZ1bmN0aW9uKGksIHYpIHtlbmNvZGVBbXAodi5zZWFyY2gpO30pO1xufSJ9LCJzZXJ2
    ZXJTaWRlIjp0cnVlLCJwcm9jZXNzaW5nIjp0cnVlfSwic2VsZWN0aW9uIjp7Im1vZGUiOiJt
    dWx0aXBsZSIsInNlbGVjdGVkIjpudWxsLCJ0YXJnZXQiOiJyb3ciLCJzZWxlY3RhYmxlIjpu
    dWxsfX0sImV2YWxzIjpbIm9wdGlvbnMuYWpheC5kYXRhIl0sImpzSG9va3MiOltdLCJkZXBz
    IjpbeyJuYW1lIjoiZHQtY29yZSIsInZlcnNpb24iOiIxLjEwLjIwIiwic3JjIjp7ImhyZWYi
    OiJkdC1jb3JlLTEuMTAuMjAifSwibWV0YSI6bnVsbCwic2NyaXB0IjoianMvanF1ZXJ5LmRh
    dGFUYWJsZXMubWluLmpzIiwic3R5bGVzaGVldCI6WyJjc3MvanF1ZXJ5LmRhdGFUYWJsZXMu
    bWluLmNzcyIsImNzcy9qcXVlcnkuZGF0YVRhYmxlcy5leHRyYS5jc3MiXSwiaGVhZCI6bnVs
    bCwiYXR0YWNobWVudCI6bnVsbCwicGFja2FnZSI6bnVsbCwiYWxsX2ZpbGVzIjpmYWxzZX0s
    eyJuYW1lIjoianF1ZXJ5IiwidmVyc2lvbiI6IjEuMTEuMyIsInNyYyI6eyJocmVmIjoianF1
    ZXJ5LTEuMTEuMyJ9LCJtZXRhIjpudWxsLCJzY3JpcHQiOiJqcXVlcnkubWluLmpzIiwic3R5
    bGVzaGVldCI6bnVsbCwiaGVhZCI6bnVsbCwiYXR0YWNobWVudCI6bnVsbCwiYWxsX2ZpbGVz
    Ijp0cnVlfSx7Im5hbWUiOiJjcm9zc3RhbGsiLCJ2ZXJzaW9uIjoiMS4xLjAuMSIsInNyYyI6
    eyJocmVmIjoiY3Jvc3N0YWxrLTEuMS4wLjEifSwibWV0YSI6bnVsbCwic2NyaXB0IjoianMv
    Y3Jvc3N0YWxrLm1pbi5qcyIsInN0eWxlc2hlZXQiOiJjc3MvY3Jvc3N0YWxrLmNzcyIsImhl
    YWQiOm51bGwsImF0dGFjaG1lbnQiOm51bGwsImFsbF9maWxlcyI6dHJ1ZX1dfQAABAIAAAL/
    AAAAEAAAAAEABAAJAAAABGpzb24AAAD+AAAEAgAAAf8AAAAQAAAAAQAEAAkAAAAmbG9hZF9m
    YWN0b3JzX21vZHVsZS1mYWN0b3JzX2luZm9fdGFibGUAAAD+AAACEwAAAAAAAAQCAAAB/wAA
    ABAAAAAAAAAA/gAABAIAAAH/AAAAEAAAAAMABAAJAAAABWlucHV0AAQACQAAAAZvdXRwdXQA
    BAAJAAAABmV4cG9ydAAAAP4=

