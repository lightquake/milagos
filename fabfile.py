from fabric.api import *

env.hosts=["amateurtopologist.com"]

def deploy():
    local("cabal-dev clean")
    local("cabal-dev configure")
    local("cabal-dev build")
    run("supervisorctl stop webdaemons:milagos")
    local("strip dist/build/milagos/milagos")
    local("scp dist/build/milagos/milagos phurst@amateurtopologist.com:/srv/milagos/")
    local("rm -rf static/tmp/*")
    for dir in "static config themes".split(" "):
        push_dir(dir)
    run("supervisorctl start webdaemons:milagos")

def push_dir(dir):
    local("rsync --progress -r %s phurst@amateurtopologist.com:/srv/milagos" % dir)
