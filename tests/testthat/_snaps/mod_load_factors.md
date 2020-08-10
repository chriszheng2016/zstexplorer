# load_factors_server - reactives and output updates

    {"x":{"filter":"none","container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>factor_code<\/th>\n      <th>factor_name<\/th>\n      <th>factor_type<\/th>\n      <th>factor_group<\/th>\n      <th>factor_description<\/th>\n      <th>factor_lag_month<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-center"},{"className":"dt-right","targets":6},{"orderable":false,"targets":0}],"pageLength":25,"order":[],"autoWidth":false,"orderClasses":false,"serverSide":true,"processing":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[],"deps":[{"name":"dt-core","version":"1.10.20","src":{"href":"dt-core-1.10.20"},"meta":null,"script":"js/jquery.dataTables.min.js","stylesheet":["css/jquery.dataTables.min.css","css/jquery.dataTables.extra.css"],"head":null,"attachment":null,"package":null,"all_files":false},{"name":"jquery","version":"1.11.3","src":{"href":"jquery-1.11.3"},"meta":null,"script":"jquery.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"crosstalk","version":"1.1.0.1","src":{"href":"crosstalk-1.1.0.1"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.css","head":null,"attachment":null,"all_files":true}]} 

---

    {"x":{"filter":"none","container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>date<\/th>\n      <th>period<\/th>\n      <th>stkcd<\/th>\n      <th>indcd<\/th>\n      <th>QR<\/th>\n      <th>CR<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-center"},{"className":"dt-right","targets":[5,6]},{"orderable":false,"targets":0}],"pageLength":25,"order":[],"autoWidth":false,"orderClasses":false,"serverSide":true,"processing":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[],"deps":[{"name":"dt-core","version":"1.10.20","src":{"href":"dt-core-1.10.20"},"meta":null,"script":"js/jquery.dataTables.min.js","stylesheet":["css/jquery.dataTables.min.css","css/jquery.dataTables.extra.css"],"head":null,"attachment":null,"package":null,"all_files":false},{"name":"jquery","version":"1.11.3","src":{"href":"jquery-1.11.3"},"meta":null,"script":"jquery.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"crosstalk","version":"1.1.0.1","src":{"href":"crosstalk-1.1.0.1"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.css","head":null,"attachment":null,"all_files":true}]} 

# load_factors_app - Module App works

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["input", "output", "export"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["load_factors_module-factor_groups", "load_factors_module-factors_in_group", "load_factors_module-factors_info_table_cell_clicked", "load_factors_module-factors_info_table_rows_all", "load_factors_module-factors_info_table_rows_current", "load_factors_module-factors_info_table_rows_selected", "load_factors_module-factors_info_table_search", "load_factors_module-factors_info_table_state", "load_factors_module-load_factors", "load_factors_module-select_factors"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["Financial Risk"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": []
                }
              },
              "value": []
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            },
            {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            },
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": [""]
            },
            {
              "type": "NULL"
            },
            {
              "type": "integer",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["integer", "shinyActionButtonValue"]
                }
              },
              "value": [1]
            },
            {
              "type": "NULL"
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["load_factors_module-factors_info_table"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["json"]
                }
              },
              "value": ["{\"x\":{\"filter\":\"none\",\"container\":\"<table class=\\\"display\\\">\\n  <thead>\\n    <tr>\\n      <th> <\\/th>\\n      <th>factor_code<\\/th>\\n      <th>factor_name<\\/th>\\n      <th>factor_type<\\/th>\\n      <th>factor_group<\\/th>\\n      <th>factor_description<\\/th>\\n      <th>factor_lag_month<\\/th>\\n    <\\/tr>\\n  <\\/thead>\\n<\\/table>\",\"options\":{\"columnDefs\":[{\"className\":\"dt-center\"},{\"className\":\"dt-right\",\"targets\":6},{\"orderable\":false,\"targets\":0}],\"pageLength\":25,\"order\":[],\"autoWidth\":false,\"orderClasses\":false,\"ajax\":{\"type\":\"POST\",\"data\":\"function(d) {\\nd.search.caseInsensitive = true;\\nd.search.smart = true;\\nd.escape = true;\\nvar encodeAmp = function(x) { x.value = x.value.replace(/&/g, \\\"%26\\\"); }\\nencodeAmp(d.search);\\n$.each(d.columns, function(i, v) {encodeAmp(v.search);});\\n}\"},\"serverSide\":true,\"processing\":true},\"selection\":{\"mode\":\"multiple\",\"selected\":null,\"target\":\"row\",\"selectable\":null}},\"evals\":[\"options.ajax.data\"],\"jsHooks\":[],\"deps\":[{\"name\":\"dt-core\",\"version\":\"1.10.20\",\"src\":{\"href\":\"dt-core-1.10.20\"},\"meta\":null,\"script\":\"js/jquery.dataTables.min.js\",\"stylesheet\":[\"css/jquery.dataTables.min.css\",\"css/jquery.dataTables.extra.css\"],\"head\":null,\"attachment\":null,\"package\":null,\"all_files\":false},{\"name\":\"jquery\",\"version\":\"1.11.3\",\"src\":{\"href\":\"jquery-1.11.3\"},\"meta\":null,\"script\":\"jquery.min.js\",\"stylesheet\":null,\"head\":null,\"attachment\":null,\"all_files\":true},{\"name\":\"crosstalk\",\"version\":\"1.1.0.1\",\"src\":{\"href\":\"crosstalk-1.1.0.1\"},\"meta\":null,\"script\":\"js/crosstalk.min.js\",\"stylesheet\":\"css/crosstalk.css\",\"head\":null,\"attachment\":null,\"all_files\":true}]}"]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": []
            }
          },
          "value": []
        }
      ]
    }

