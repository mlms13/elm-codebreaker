var gulp = require('gulp'),
    gutil = require('gulp-util'),

    // gulp plugins and utilities
    stylus = require('gulp-stylus'),

    // configuration
    paths = {
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

gulp.task('build', ['stylus']);

gulp.task('watch', ['build'], function () {
  gulp.watch(paths.stylus.all, ['stylus']);
});


gulp.task('default', ['watch']);
