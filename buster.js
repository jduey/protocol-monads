var config = exports;

config['Browser Tests'] = {
    environment: 'browser',
    sources: [],
    tests: [ "resources/js/protocol_monads_browser_test.js"
           , "resources/js/protocol_monads_browser_optimized_test.js" ]
};

config['Node Tests'] = {
    environment: 'node',
    sources: [],
    tests: ["resources/js/protocol_monads_node_test.js"]
};
