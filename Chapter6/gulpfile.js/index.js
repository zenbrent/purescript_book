// modified from http://blog.sigmapoint.pl/purescript-will-make-you-purr-like-a-kitten/
//
// TODO: browserify! and tests!

var del = require("del");
var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var watch = require("gulp-watch");
var notify = require("gulp-notify");
var _ = require("highland");

var src = [
    "src/**/*.purs",
    "bower_components/**/src/**/*.purs"
];

var watchSrc = src.concat(["!src/**/Main.purs"]);

var tests = [
    "tests/*.purs",
    "bower_components/**/src/**/*.purs"
];

var dest = "dist";

var watchOptions = {verbose: true};

function psCompile(notification) {
    return function() {
        return gulp
        .src(src)
        .pipe(plumber())
        .pipe(purescript.pscMake({output: dest}))
        .pipe(_().last())
        .pipe(notify("purescript:node done"));
    };
}

function psciCompile(notification) {
    return function() {
        return gulp
        .src(watchSrc, watchOptions)
        .pipe(plumber())
        .pipe(purescript.dotPsci())
        .pipe(_().last())
        .pipe(notify(notification));
    };
}

gulp.task("purescript:node", psCompile("purescript:node done"));

gulp.task("purescript:node:watch", function() {
    return watch(src, watchOptions,
                 psCompile("purescript:node:watch done"));
});

gulp.task("purescript:psci", psciCompile("purescript:psci done"));

gulp.task("purescript:psci:watch", function() {
    return watch(watchSrc, watchOptions,
                 psciCompile("purescript:psci:watch done"));
});

gulp.task("clean", function(cb) {
    del(dest, cb);
});

gulp.task("compile", ["purescript:node", "purescript:psci"]);
gulp.task("watch", ["purescript:node:watch", "purescript:psci:watch"]);
gulp.task("default", ["compile", "watch"]);
