#!/bin/bash -e

echo ""
echo "----------------------------------------------"
echo "pushing docker image..."
echo "----------------------------------------------"
echo ""

echo $DOCKER_PASSWORD | docker login -u $DOCKER_USERNAME --password-stdin
docker push qccoders/8ball
docker logout

echo ""
echo "----------------------------------------------"
echo "deploying to host..."
echo "----------------------------------------------"
echo ""

sshpass -p $EIGHTBALL_HOST_PASSWORD ssh -o StrictHostKeyChecking=no root@8ball.qccoders.org '
    docker rm 8ball --force;
    docker pull qccoders/8ball;
    docker run -dp 80:8080 --name 8ball qccoders/8ball;
    exit;
'

echo ""
echo "done!"
echo ""
