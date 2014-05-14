from slugify import slugify
import os
import sys
import time


def parse_header(lines):
    separator = '---'
    assert (lines[0].strip() == separator)
    header = {}
    for l in lines[1:]:
        l = l.strip()
        if l == separator:
            return header
        k, v = l.split(':')
        header[k] = v.strip()
    assert False


def main():
    fname = sys.argv[1]
    with open(fname) as f:
        contents = f.readlines()
    header = parse_header(contents)
    title = header['title']
    slug = slugify(title)
    today = time.strftime("%Y-%m-%d")
    slugtitle = '{date}-{slug}.mdwn'.format(date=today, slug=slug)
    dest = os.path.join('posts', slugtitle)
    os.rename(fname, dest)

if __name__ == '__main__':
    main()