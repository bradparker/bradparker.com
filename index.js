var Metalsmith   = require('metalsmith'),
    serve        = require('metalsmith-serve'),
    watch        = require('metalsmith-watch'),
    ignore       = require('metalsmith-ignore'),
    markdown     = require('metalsmith-markdown'),
    templates    = require('metalsmith-templates'),
    collections  = require('metalsmith-collections'),
    autoprefixer = require('metalsmith-autoprefixer'),
    sass         = require('metalsmith-sass')

var metalsmith = Metalsmith(__dirname)

metalsmith
  .source('./source')
  .use(ignore(['_*.scss']))
  .use(serve())
  .use(watch({
    "${source}/**/*": "**/*"
  }))
  .use(collections({
    works: { pattern: 'works/*.md' }
  }))
  .use(sass())
  .use(autoprefixer())
  .use(markdown())
  .use(templates('mustache'))
  .build(function(err) {
    if (err) throw err
  })
