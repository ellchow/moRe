from rocker/verse:latest

run R -e "install.packages('hash', repos = 'https://cran.cnr.berkeley.edu/')"
run R -e "install.packages('rjson', repos = 'https://cran.cnr.berkeley.edu/')"
run R -e "install.packages('doMC', repos = 'https://cran.cnr.berkeley.edu/')"
run R -e "install.packages('listviewer', repos = 'https://cran.cnr.berkeley.edu/')"
run R -e "install.packages('XML', repos = 'https://cran.cnr.berkeley.edu/')"
run R -e "install.packages('sfsmisc', repos = 'https://cran.cnr.berkeley.edu/')"

# docker build -t my-r .
# docker run -a stdin -a stdout -i -t --tmpfs /tmp my-r R



# IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
# xhost + $IP
# R_DOCKER_START_CMD="docker run -a stdin -a stdout -e DISPLAY=$IP:0 -v /tmp/.X11-unix:/tmp/.X11-unix -e HOME=/home/`whoami` -e R_HISTFILE=/home/`whoami`/.Rhistory -v $HOME:/home/`whoami` -i -t my-r R --no-save"
# alias R=$R_DOCKER_START_CMD
