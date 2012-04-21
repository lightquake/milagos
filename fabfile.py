from fabric.api import *

env.hosts=["amateurtopologist.com"]

def deploy():
    local("cabal-dev clean")
    local("cabal-dev configure")
    local("cabal-dev build")
    run("supervisorctl stop webdaemons:milagos")
    local("scp cabal-dev/bin/milagos phurst@amateurtopologist.com:/srv/milagos/")
    local("rm -rf static/tmp/*")
    local("rsync --progress -r static phurst@amateurtopologist.com:/srv/milagos/static")
    local("rsync --progress -r config phurst@amateurtopologist.com:/srv/milagos/config")
    run("supervisorctl start webdaemons:milagos")
