module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({

        libFiles: [
            "bower_components/purescript-*/src/**/*.purs",
            "src/**/*.purs"
        ],

        testFiles: [
            "tests/Tests.purs",
            "<%=libFiles%>"
        ],

        clean: ["tmp", "output"],

        pscMake: {
            lib: {
                src: ["<%=libFiles%>"],
                main: "Chapter2",
                modules: ["Chapter2"],
            },
            tests: {
                src: ["<%=testFiles%>"]
            },
            all: {
                dest: "output/"
            }
        },

        dotPsci: ["<%=libFiles%>"],

        copy: [
            {
                expand: true,
                cwd: "output",
                src: ["**"],
                dest: "tmp/node_modules/"
            }, {
                src: ["js/index.js"],
                dest: "tmp/index.js"
            }
        ],

        execute: {
            tests: {
                src: "tmp/index.js"
            }
        },

        watch: {
            files: ["<%= libFiles %>", "<%= testFiles %>"],
            tasks: ["clean", "make", "test"]
        }
    });

    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-execute");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-watch");

    grunt.registerTask("test", ["pscMake:tests", "copy", "execute:tests"]);
    grunt.registerTask("make", ["pscMake:lib", "dotPsci"]);
    grunt.registerTask("default", ["clean", "make", "test"]);
};
