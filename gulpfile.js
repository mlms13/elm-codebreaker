var gulp = require('gulp'),
    gutil = require('gulp-util'),

    // gulp plugins and utilities
    stylus = require('gulp-stylus'),
    uglify = require('gulp-uglify'),

    // configuration
    paths = {
      js: {
        all: './www/**/*.js',
        dest: './www'
      },
      stylus: {
        main: './assets/styl/index.styl',
        all: ['./assets/styl/**/*.styl'],
        dest: './www'
      },
    };

gulp.task('stylus', function () {
  return gulp.src(paths.stylus.main)
    .pipe(stylus())
    .pipe(gulp.dest(paths.stylus.dest));
});

gulp.task("uglify-js", function () {
  return gulp.src(paths.js.all)
    .pipe(uglify())
    .pipe(gulp.dest(paths.js.dest));
});

gulp.task('build', ['stylus', 'uglify-js']);

gulp.task('watch', ['build'], function () {
  gulp.watch(paths.stylus.all, ['stylus']);
  gulp.watch(paths.js.all, ['uglify-js']);
});


gulp.task('default', ['watch']);
