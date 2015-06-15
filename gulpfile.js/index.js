// modified from http://blog.sigmapoint.pl/purescript-will-make-you-purr-like-a-kitten/
//
// TODO: browserify! and tests!

var del = require("del");
var gulp = require("gulp");
var purescript = require("gulp-purescript");
var watch = require("gulp-watch");

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

function pscMake() {
    return purescript.pscMake({output: dest});
}

function dotPsci() {
    return purescript.dotPsci();
}

gulp.task("purescript:node", function() {
    return gulp.src(src).pipe(pscMake())
});

gulp.task("purescript:node:watch", function() {
    return watch(src, watchOptions, function() {
        return gulp.src(src).pipe(pscMake())
    });
});


gulp.task("purescript:psci", function() {
    return gulp.src(watchSrc, watchOptions).pipe(dotPsci());
});

gulp.task("purescript:psci:watch", function() {
    return watch(watchSrc, watchOptions, function() {
        return gulp.src(watchSrc, watchOptions).pipe(dotPsci());
    });
});


gulp.task("clean", function(cb) {
    del(dest, cb);
});

gulp.task("compile", ["purescript:node", "purescript:psci"]);
gulp.task("watch", ["purescript:node:watch", "purescript:psci:watch"]);
gulp.task("default", ["compile", "watch"]);
