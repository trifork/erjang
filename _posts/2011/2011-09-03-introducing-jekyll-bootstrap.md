---

title: Introducing Jekyll Bootstrap
layout: default

---

Jekyll bootstrap is a theme for jekyll which uses the [twitter bootstrap css framework](https://github.com/twitter/bootstrap).
The advantage of jekyll is that it is self hosted (or hosted on github) and that you can write in markdown on a text editor and just git push new posts to a server which produces html files. The advantage of this theme is so that you can start blogging almost right away without having to worry about making a theme.

###Setting up
To start you own blog, simply `git clone` the [repository on github](https://github.com/nhoss2/jekyll-bootstrap). You could also press the "fork" button on github.

	git clone git://github.com/nhoss2/jekyll-bootstrap.git

If you want to have your blog on github, make sure you change to the `gh-pages` branch.

	git checkout gh-pages

Then you will need to edit the `_config.yml` file at the root of repository.

To add your own posts, add a file to the `_posts` directory which has the name `year-month-day-title.md`. Note - the file does not have to markdown.

To publish the post, just `git push` it to your own github repo and your set!

###Things to change on `_config.yml`
There is a config file at the root called `_config.yml`. By Default it looks like:

	permalink: /:year/:title/
	paginate: 10
	exclude:
	name: Jekyll Bootstrap
	baseurl: /jekyll-bootstrap/

You will need to change the `name` and `baseurl` fields. The others are optional.
The `baseurl` field is used for the css files and pagination, if you are hosting the blog on github, you will need to change it to your repository name unless your repository is the same name as your github user name, which means you will need to have no value for `baseurl`.

For more information on Jekyll, visit their [wiki on github](https://github.com/mojombo/jekyll/wiki).

For more information on github pages: [http://pages.github.com](http://pages.github.com).
