# Using Milagos

First, pick a theme and put it in `themes/themename/themename.css`; the github repository has a sample theme named `carbonated`. Note that the CSS filename and directory name *must* match.

Then, create `posts/` and `pages/` subdirectories under wherever you put the binary, and run it. To create a post, just make a subdirectory under *that* containing a `meta.yml` file that looks like

    title: My Awesome Post
    slug: my-awesome-post-slug
    posted: 2012-05-02 18:00:00 EDT
    tags:
     - meta

and write markdown in `post.markdown`! The post is automatically refreshed every time you save, no need for manual restarts. If you set `draft: true`, the post won't show up on the main page or in any tags, but you can still directly go to it by visiting, say, `localhost:8000/2012/05/02/a-draft`.

# Customizing Milagos

Unless you want to use the exact same theme I use, you're going to
want to customize the theme.

1. Clone the repository, and cabal-dev install it. (I personally recommend using [sandboxer](https://github.com/veinor/sandboxer) to create a new sandbox, but I'm probably biased because I wrote it).

2. Catch up on your newsfeed. A completely fresh cabal install of Milagos and all its dependencies with profiling libs took about 15 minutes on my 3.0 GHz Athlon II X4 640. Obviously, if you don't enable profiling, it'll take about half that.

3. Copy `config/example.settings.yml` to `config/settings.yml` and customize appropriately.

4. Edit `fabfile.py` so that you don't try deploying to my server. :)

5. Change `templates/footer.hamlet` to not credit your posts to me! It'd be nice if you left the 'powered by' links in, though.

5. Go play around in `templates/`. The file names are hopefully self-explanatory; post-list renders a list of posts, post renders an individual post, tag-list renders the tag list on the sidebar, etc. You can mostly ignore default-layout-wrapper unless you want to add stuff to the `<head>` tags.

6. If you encounter any issues, [file them](https://github.com/veinor/milagos)! If you have any comments or feedback that's not appropriate for the issue tracker, [e-mail me](mailto:phurst@amateurtopologist.com).

7. Have fun!

## Theming

Currently, theme support is limited; the filename of the
CSS file must be the same as the name of the folder that contains it,
and you can only do CSS. Eventually, I do plan on allowing customizing
the templates themselves through themes.
