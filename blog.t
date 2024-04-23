  $ ./blog.exe -i . -o output

  $ find output -type f | sort | while read f ; do sha256sum $f ; done
  d699f303990ce9bd7d7c97e9bd3cad6a46ecf2532f475cf22ae58213237821b9  output/css/bootstrap.css
  1cd5618d98ea76b4fe78afc2589907c0ad4a73a3f8d72193246609b88e7a06b4  output/css/extra.css
  c0a1f4036df57808b4f2e30d3512f62608f5ec2f7d419657090a039ae540853b  output/css/syntax.css
  8254e45ea4651b4aa1b9f7683b571c64cfb9af75e794d10103391469e1bb7ffe  output/favicon.ico
  f495f34e4f177cf0115af995bbbfeb3fcabc88502876e76fc51a4ab439bc8431  output/fonts/glyphicons-halflings-regular.eot
  5d234508037dc13a419ef6ce48f3fc73dbb477f1a162c052b872182b494e626e  output/fonts/glyphicons-halflings-regular.svg
  bd18efd3efd70fec8ad09611a20cdbf99440b2c1d40085c29be036f891d65358  output/fonts/glyphicons-halflings-regular.ttf
  fc969dc1c6ff531abcf368089dcbaf5775133b0626ff56b52301a059fc0f9e1e  output/fonts/glyphicons-halflings-regular.woff
  8942a9a16b6d37c9013a53a233f7b4fd2f40be274b80b5a741b8323b862ead08  output/img/debian.png
  4c924a863056cabb8072b368516552577bf67861ce3b5322fd37945907148f6f  output/img/github.png
  895d6b598afdcd51b9552654b012e3ef6578d44e3e0efdb59d33024ead80a5a4  output/img/grey_wash_wall.png
  50d3007f6bb3d3814f80c42ac716fd68988172ea545e9d5c784e3e1eea84096d  output/img/linkedin.png
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2011-11-11-hello-world.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2011-11-21-hakyll-101.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2011-11-28-unicode-math-greek-symbols-you-name-it.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2011-12-14-what-s-in-an-adt.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2012-01-17-zsh-suffix-aliases.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2012-08-30-stripe-ctf-2.0.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-05-13-resizing-a-lvm-partition.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-05-21-making-type-inference-explode.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-06-05-bring-your-own-switch.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-11-21-my-part-of-work-in-debian-jessie.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-11-27-converting-a-dance-dance-revolution-mat-to-usb.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2014-12-27-on-the-curl-sh-pattern.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2015-04-03-santa-made-me-learn-rails-in-a-week.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2016-01-12-in-python-default-values-are-evaluated-at-import-time.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2017-02-01-nabomamo-2016-writeup.mdwn
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/posts/2020-08-03-fuzzing-ocamlformat-with-afl-and-crowbar.mdwn
  e5c4b84484ee4216e9373be99380320c25dd94805f99f0a805846f087636553f  output/robots.txt
  6a67a2d3087bbdb898d7178dabb7cdccb9f8cdfd04723e555c64b8622e75a601  output/rss.xml
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/avr
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/bot
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/ctf
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/ddr
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/debian
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/fuzzing
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/haskell
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/linux
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/lvm
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/meta
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/nabomamo
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/ocaml
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/python
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/rails
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/secretsanta
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/security
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/slack
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/stripe
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/tex
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/types
  01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b  output/tags/zsh

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
  

  $ cat output/tags/ocaml
  
