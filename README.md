# Craywatch project website

This repository contains the source files for the [Craywatch website](https://craywatch.inbo.be), as well as the analysis workflow of the resulting report.

## Usage

This website makes use of the static website generator [Jekyll](https://jekyllrb.com/) and the [Petridish](https://github.com/peterdesmet/petridish) theme. **Each commit to `main` will automatically trigger a new build and deployment via the [GitHub Actions workflow](.github/workflows/pages.yml).** For this to work, the repository setting `Settings > Pages > Build and deployment > Source` must be set to `GitHub Actions`.

### CARTO basemaps API key

The maps use the [CARTO Positron basemap](https://docs.carto.com/faqs/carto-basemaps), which requires a (free) API key. The key is stored as the repository secret `CARTO_API_KEY` and is written by the workflow to a `_config_secrets.yml` file, which is used as an extra Jekyll config file at build time. The key is never committed to this repository. Note that the key does end up in the generated HTML, as the browser needs it to request the map tiles.

### Build the site locally

There is no need to build the site locally, but you can by installing Jekyll and running:

```bash
bundle exec jekyll serve
```

Without an API key the maps fall back to the OpenStreetMap basemap. To test the CARTO basemap locally, create a `_config_secrets.yml` file in the root of the repository (it is git ignored):

```yaml
carto_api_key: "your-carto-api-key"
```

And run:

```bash
bundle exec jekyll serve --config _config.yml,_config_secrets.yml
```

## Repo structure

The repository structure follows that of Jekyll websites.

- General site settings: [_config.yml](_config.yml)
- Pages: [pages/](pages/)
- Posts: [_posts/](_posts/)
- Images & static files: [assets/](assets/)
- Top navigation: [_data/navigation.yml](_data/navigation.yml)
- Footer content: [_data/footer.yml](_data/footer.yml)
- Team members: [_data/team.yml](_data/team.yml)

## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).
