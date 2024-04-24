  $ ./blog.exe -i . -o output

  $ find output -type f | sort
  output/css/bootstrap.css
  output/css/extra.css
  output/css/syntax.css
  output/favicon.ico
  output/fonts/glyphicons-halflings-regular.eot
  output/fonts/glyphicons-halflings-regular.svg
  output/fonts/glyphicons-halflings-regular.ttf
  output/fonts/glyphicons-halflings-regular.woff
  output/img/debian.png
  output/img/github.png
  output/img/grey_wash_wall.png
  output/img/linkedin.png
  output/posts/2011-11-11-hello-world.mdwn
  output/posts/2011-11-21-hakyll-101.mdwn
  output/posts/2011-11-28-unicode-math-greek-symbols-you-name-it.mdwn
  output/posts/2011-12-14-what-s-in-an-adt.mdwn
  output/posts/2012-01-17-zsh-suffix-aliases.mdwn
  output/posts/2012-08-30-stripe-ctf-2.0.mdwn
  output/posts/2014-05-13-resizing-a-lvm-partition.mdwn
  output/posts/2014-05-21-making-type-inference-explode.mdwn
  output/posts/2014-06-05-bring-your-own-switch.mdwn
  output/posts/2014-11-21-my-part-of-work-in-debian-jessie.mdwn
  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb.mdwn
  output/posts/2014-12-27-on-the-curl-sh-pattern.mdwn
  output/posts/2015-04-03-santa-made-me-learn-rails-in-a-week.mdwn
  output/posts/2016-01-12-in-python-default-values-are-evaluated-at-import-time.mdwn
  output/posts/2017-02-01-nabomamo-2016-writeup.mdwn
  output/posts/2020-08-03-fuzzing-ocamlformat-with-afl-and-crowbar.mdwn
  output/robots.txt
  output/rss.xml
  output/tags/avr
  output/tags/bot
  output/tags/ctf
  output/tags/ddr
  output/tags/debian
  output/tags/fuzzing
  output/tags/haskell
  output/tags/linux
  output/tags/lvm
  output/tags/meta
  output/tags/nabomamo
  output/tags/ocaml
  output/tags/python
  output/tags/rails
  output/tags/secretsanta
  output/tags/security
  output/tags/slack
  output/tags/stripe
  output/tags/tex
  output/tags/types
  output/tags/zsh

  $ cat output/rss.xml
  <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:dc="http://purl.org/dc/elements/1.1/"><channel><title>Enter the void *</title><link>http://blog.emillon.org</link><description>
  <![CDATA[
  Yet another random hacker
  ]]>
  </description><atom:link href="http://blog.emillon.org/rss.xml" rel="self" type="application/rss+xml"></atom:link><lastBuildDate>Fri, 11 Nov 2011 00:00:00 UT</lastBuildDate><item><title>Hello, world !</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Fri, 11 Nov 2011 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Hakyll 101</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Mon, 21 Nov 2011 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Unicode : Math, greek, symbols - you name it !</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Mon, 28 Nov 2011 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>What's in an ADT ?</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Wed, 14 Dec 2011 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>ZSH suffix aliases</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Tue, 17 Jan 2012 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Stripe CTF 2.0 (partial) writeup</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Thu, 30 Aug 2012 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Resizing a LVM partition</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Tue, 13 May 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Making type inference explode</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Wed, 21 May 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Bring your own switch</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Thu, 5 Jun 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>My part of work in Debian Jessie</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Fri, 21 Nov 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Converting a Dance Dance Revolution mat to USB</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Thu, 27 Nov 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>On the curl | sh pattern</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Sat, 27 Dec 2014 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Santa made me learn Rails in a week</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Fri, 3 Apr 2015 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>In Python, default values are evaluated at import time</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Tue, 12 Jan 2016 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>NaBoMaMo 2016 writeup</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Wed, 1 Feb 2017 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item><item><title>Fuzzing OCamlFormat with AFL and Crowbar</title><link>permalink</link><description>
  <![CDATA[
  description
  ]]>
  </description><pubDate>Mon, 3 Aug 2020 00:00:00 UT</pubDate><guid>permalink</guid><dc:creator>Etienne Millon</dc:creator></item></channel></rss>

  $ cat output/posts/2011-11-11-hello-world.mdwn
  <h1>Hello, world !</h1>
  
  <p>by <em>Etienne Millon</em> on <strong>Fri, 11 Nov 2011 00:00:00 UT</strong></p>
  
  <p>Tagged as: meta.</p>
  
  BODY
  
  
  <hr>
  
  You can see the comments about this article
  <a href="https://news.ycombinator.com/item?id=HN">on Hacker News</a>.
  

  $ cat output/tags/ocaml
  
