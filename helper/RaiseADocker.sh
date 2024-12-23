# Change the docker container id to the one you want to raise

$docker = "50980d574903"

sudo systemctl start docker
sudo docker restart $docker
sudo docker attach $docker