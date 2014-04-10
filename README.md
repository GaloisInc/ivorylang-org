# [SMACCMPilot-org][smaccmpilotorg]

Sources for the [smaccmpilot.org][] website.

## Bugs and Pull Requests

We encourage users to file bugs or pull requests regarding any problems with the
site - anything from incorrect, confusing, or missing documentation to dead
links and spelling mistakes is a tremendous help.

Thank you! - Pat and the SMACCMPilot team.

## Contents

Each page of smaccmpilot.org is rendered from a [Markdown][] document. Page
sources are found under the `/pages/` directory.

The rendering template for standard pages is found in `/templates/standard.html`,
and the `/index.html` landing page is rendered from `/templates/frontpage.html`.

## Building

smaccmpilot.org is a static web site generated using the excellent [`hakyll`][]
package. The included `Makefile` will do a complete build of the site in a cabal
sandbox. You will need ghc 7.4 or higher, and cabal 1.18 or higher.

The `make` command will install the complete site generator executable in
`/.cabal-sandbox` and then generate the static pages into `/_site/`. The
`make preview` command will automatically rebuild pages each time a markdown
source is edited, and run a web server for previewing the site on
`http://localhost:8000`.

The site generator executable only needs to be rebuilt when editing the
top navigation bar, side navigation tree, or subdirectory structure in
`/pages/`. These are all described in `site.hs`.

## Other Sources

Sources from the jquery and bootstrap projects are distributed in this
repository, they retain their original copyright and license.

[smaccmpilotorg]: http://github.com/GaloisInc/smaccmpilot-org
[smaccmpilot.org]: http://smaccmpilot.org

[hakyll]: http://jaspervdj.be/hakyll/
[Markdown]: http://daringfireball.net/projects/markdown/syntax

