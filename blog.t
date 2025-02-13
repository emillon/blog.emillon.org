  $ pp_html () {
  >   tidy -q --indent yes "$1"
  > }

  $ pp_xml () {
  >   xmllint --pretty 1 "$1"
  > }

  $ ./blog.exe -i . -o output

  $ find output -type f | sort
  output/css/bootstrap.css
  output/css/extra.css
  output/css/syntax.css
  output/favicon.ico
  output/feeds/algebra.xml
  output/feeds/avr.xml
  output/feeds/bot.xml
  output/feeds/comonad.xml
  output/feeds/conway.xml
  output/feeds/ctf.xml
  output/feeds/ddr.xml
  output/feeds/debian.xml
  output/feeds/dune.xml
  output/feeds/emulator.xml
  output/feeds/fuzzing.xml
  output/feeds/hacking-days.xml
  output/feeds/haskell.xml
  output/feeds/lenses.xml
  output/feeds/life.xml
  output/feeds/linux.xml
  output/feeds/lvm.xml
  output/feeds/meta.xml
  output/feeds/nabomamo.xml
  output/feeds/ocaml.xml
  output/feeds/python.xml
  output/feeds/rails.xml
  output/feeds/secretsanta.xml
  output/feeds/security.xml
  output/feeds/slack.xml
  output/feeds/stripe.xml
  output/feeds/tex.xml
  output/feeds/tree-sitter.xml
  output/feeds/types.xml
  output/feeds/zsh.xml
  output/fonts/glyphicons-halflings-regular.eot
  output/fonts/glyphicons-halflings-regular.svg
  output/fonts/glyphicons-halflings-regular.ttf
  output/fonts/glyphicons-halflings-regular.woff
  output/img/debian.png
  output/img/github.png
  output/img/grey_wash_wall.png
  output/img/linkedin.png
  output/index.html
  output/posts.html
  output/posts/2011-11-11-hello-world.html
  output/posts/2011-11-21-hakyll-101.html
  output/posts/2011-11-28-unicode-math-greek-symbols-you-name-it.html
  output/posts/2011-12-14-what-s-in-an-adt.html
  output/posts/2012-01-17-zsh-suffix-aliases.html
  output/posts/2012-08-30-stripe-ctf-2.0.html
  output/posts/2012-10-18-comonadic-life.html
  output/posts/2014-05-13-resizing-a-lvm-partition.html
  output/posts/2014-05-21-making-type-inference-explode.html
  output/posts/2014-06-05-bring-your-own-switch.html
  output/posts/2014-11-21-my-part-of-work-in-debian-jessie.html
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb.html
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb/assembled.jpg
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb/inside.jpg
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb/mat.jpg
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb/wires.jpg
  output/posts/2014-12-27-on-the-curl-sh-pattern.html
  output/posts/2015-04-03-santa-made-me-learn-rails-in-a-week.html
  output/posts/2015-04-03-santa-made-me-learn-rails-in-a-week/nested.png
  output/posts/2015-08-20-a-lens-based-st20-emulator.html
  output/posts/2015-08-20-a-lens-based-st20-emulator/t00f.png
  output/posts/2015-08-20-a-lens-based-st20-emulator/t03.png
  output/posts/2016-01-12-in-python-default-values-are-evaluated-at-import-time.html
  output/posts/2017-02-01-nabomamo-2016-writeup.html
  output/posts/2020-08-03-fuzzing-ocamlformat-with-afl-and-crowbar.html
  output/posts/2024-07-26-introducing-tree-sitter-dune.html
  output/robots.txt
  output/rss.xml
  output/tags/algebra.html
  output/tags/avr.html
  output/tags/bot.html
  output/tags/comonad.html
  output/tags/conway.html
  output/tags/ctf.html
  output/tags/ddr.html
  output/tags/debian.html
  output/tags/dune.html
  output/tags/emulator.html
  output/tags/fuzzing.html
  output/tags/hacking-days.html
  output/tags/haskell.html
  output/tags/lenses.html
  output/tags/life.html
  output/tags/linux.html
  output/tags/lvm.html
  output/tags/meta.html
  output/tags/nabomamo.html
  output/tags/ocaml.html
  output/tags/python.html
  output/tags/rails.html
  output/tags/secretsanta.html
  output/tags/security.html
  output/tags/slack.html
  output/tags/stripe.html
  output/tags/tex.html
  output/tags/tree-sitter.html
  output/tags/types.html
  output/tags/zsh.html

  $ pp_xml output/rss.xml
  <?xml version="1.0"?>
  <rss xmlns:atom="http://www.w3.org/2005/Atom" xmlns:dc="http://purl.org/dc/elements/1.1/" version="2.0">
    <channel>
      <title>Enter the void *</title>
      <link>http://blog.emillon.org</link>
      <description><![CDATA[
  Yet another random hacker
  ]]></description>
      <atom:link href="http://blog.emillon.org/rss.xml" rel="self" type="application/rss+xml"/>
      <lastBuildDate>Fri, 11 Nov 2011 00:00:00 UT</lastBuildDate>
      <item>
        <title>Hello, world !</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Fri, 11 Nov 2011 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Hakyll 101</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Mon, 21 Nov 2011 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Unicode : Math, greek, symbols - you name it !</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Mon, 28 Nov 2011 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>What's in an ADT ?</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Wed, 14 Dec 2011 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>ZSH suffix aliases</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Tue, 17 Jan 2012 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Stripe CTF 2.0 (partial) writeup</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Thu, 30 Aug 2012 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Comonadic Life</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Thu, 18 Oct 2012 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Resizing a LVM partition</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Tue, 13 May 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Making type inference explode</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Wed, 21 May 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Bring your own switch</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Thu, 5 Jun 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>My part of work in Debian Jessie</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Fri, 21 Nov 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Converting a Dance Dance Revolution mat to USB</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Thu, 27 Nov 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>On the curl | sh pattern</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Sat, 27 Dec 2014 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Santa made me learn Rails in a week</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Fri, 3 Apr 2015 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>A lens-based ST20 emulator</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Thu, 20 Aug 2015 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>In Python, default values are evaluated at import time</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Tue, 12 Jan 2016 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>NaBoMaMo 2016 writeup</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Wed, 1 Feb 2017 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Fuzzing OCamlFormat with AFL and Crowbar</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Mon, 3 Aug 2020 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
      <item>
        <title>Introducing tree-sitter-dune</title>
        <link>permalink</link>
        <description><![CDATA[
  description
  ]]></description>
        <pubDate>Fri, 26 Jul 2024 00:00:00 UT</pubDate>
        <guid>permalink</guid>
        <dc:creator>Etienne Millon</dc:creator>
      </item>
    </channel>
  </rss>

  $ pp_html output/posts/2011-11-11-hello-world.html
  line 1 column 1 - Warning: missing <!DOCTYPE> declaration
  line 1 column 1 - Warning: inserting implicit <body>
  line 1 column 1 - Warning: inserting missing 'title' element
  <!DOCTYPE html>
  <html>
    <head>
      <meta name="generator" content=
      "HTML Tidy for HTML5 for Linux version 5.8.0">
      <title></title>
    </head>
    <body>
      <h1>
        Hello, world !
      </h1>
      <p>
        by <em>Etienne Millon</em> on <strong>Fri, 11 Nov 2011
        00:00:00 UT</strong>
      </p>
      <p>
        Tagged as: meta.
      </p>
      <hr>
      <h2>
        title: Hello, world ! author: Etienne Millon tags: meta
      </h2>
      <pre><code class="language-{.haskell}">blog :: IO ()
  blog =
    putStrLn "Hello, world !"
  </code></pre>
      <p>
        This is my first attempt at blogging, I still don't know what
        to expect. I will probably write about the following topics :
      </p>
      <ul>
        <li>Programming, especially using <a href=
        "http://caml.inria.fr/">functional</a> <a href=
        "http://www.haskell.org/">languages</a>.
        </li>
        <li>Development of the <a href=
        "http://www.debian.org/">Debian</a> operating system.
        </li>
        <li>
          <a href=
          "http://en.wikipedia.org/wiki/Static_program_analysis">Static
          analysis</a> of software.
        </li>
        <li>Computer security.
        </li>
      </ul>
      <p>
        Like some of <a href=
        "http://blog.chmd.fr/going-static.html">my</a> <a href=
        "http://nicdumz.fr/blog/2010/12/why-blogofile/">friends</a>,
        I decided to use a static blog generator. The first series of
        posts will be about setting this up with <a href=
        "http://jaspervdj.be/hakyll/">hakyll</a>, git and S3. Stay
        tuned !
      </p>
    </body>
  </html>
  [1]

  $ pp_html output/tags/ocaml.html
  line 1 column 1 - Warning: missing <!DOCTYPE> declaration
  line 1 column 1 - Warning: inserting implicit <body>
  line 1 column 1 - Warning: inserting missing 'title' element
  <!DOCTYPE html>
  <html>
    <head>
      <meta name="generator" content=
      "HTML Tidy for HTML5 for Linux version 5.8.0">
      <title></title>
    </head>
    <body>
      <h1>
        Posts tagged "ocaml"
      </h1>
      <ul class="postList">
        <li>
          <span class="postDate">Fri, 26 Jul 2024 00:00:00 UT</span>
          <a href="permalink">Introducing tree-sitter-dune</a>
        </li>
        <li>
          <span class="postDate">Mon, 3 Aug 2020 00:00:00 UT</span>
          <a href="permalink">Fuzzing OCamlFormat with AFL and
          Crowbar</a>
        </li>
        <li>
          <span class="postDate">Wed, 1 Feb 2017 00:00:00 UT</span>
          <a href="permalink">NaBoMaMo 2016 writeup</a>
        </li>
        <li>
          <span class="postDate">Wed, 21 May 2014 00:00:00 UT</span>
          <a href="permalink">Making type inference explode</a>
        </li>
        <li>
          <span class="postDate">Wed, 14 Dec 2011 00:00:00 UT</span>
          <a href="permalink">What's in an ADT ?</a>
        </li>
      </ul><a href="http://blog.emillon.org/tag/ocaml.xml">Feed</a>
    </body>
  </html>
  [1]

  $ pp_html output/index.html
  line 1 column 1 - Warning: missing <!DOCTYPE> declaration
  line 1 column 1 - Warning: inserting implicit <body>
  line 1 column 1 - Warning: inserting missing 'title' element
  <!DOCTYPE html>
  <html>
    <head>
      <meta name="generator" content=
      "HTML Tidy for HTML5 for Linux version 5.8.0">
      <title></title>
    </head>
    <body>
      <div class="about">
        <h1>
          About this blog
        </h1>
        <p>
          Hello! I am a French computer scientist specialized in
          formal methods.
        </p>
        <p>
          This blog is mostly about security, types, functional
          programming and free software.
        </p>
        <div id="posts">
          I recently wrote about the following things:
          <ul class="postList">
            <li>
              <span class="postDate">Fri, 11 Nov 2011 00:00:00
              UT</span> <a href="permalink">Hello, world !</a>
            </li>
            <li>
              <span class="postDate">Mon, 21 Nov 2011 00:00:00
              UT</span> <a href="permalink">Hakyll 101</a>
            </li>
            <li>
              <span class="postDate">Mon, 28 Nov 2011 00:00:00
              UT</span> <a href="permalink">Unicode : Math, greek,
              symbols - you name it !</a>
            </li>
          </ul>
          <p>
            <a href="/posts.html">See the rest…</a>
          </p>
        </div>
        <h1>
          Tags
        </h1>
        <div>
          TAGCLOUD
        </div>
      </div>
    </body>
  </html>
  [1]
