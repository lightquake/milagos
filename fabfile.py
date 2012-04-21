from fabric.api import *

env.hosts=["amateurtopologist.com"]

def deploy():
    local("cabal-dev clean")
    local("cabal-dev configure")
    local("cabal-dev build")
    run("supervisorctl stop webdaemons:milagos")
    local("scp dist/build/milagos/milagos phurst@amateurtopologist.com:/srv/milagos/")
    local("rm -rf static/tmp/*")
    local("rsync --progress -r static phurst@amateurtopologist.com:/srv/milagos")
    local("rsync --progress -r config phurst@amateurtopologist.com:/srv/milagos")
    run("supervisorctl start webdaemons:milagos")
