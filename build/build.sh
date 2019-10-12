#!/bin/bash -e

cd web

echo ""
echo "----------------------------------------------"
echo "building web..."
echo $PWD
echo "----------------------------------------------"
echo ""

yarn install
yarn run build

echo ""
echo "----------------------------------------------"
echo "copying static files to ../hub/public..."
echo "----------------------------------------------"
echo ""

cp -r build/. ../hub/public
ls -R ../hub/public | awk '
    /:$/&&f{s=$0;f=0}
    /:$/&&!f{sub(/:$/,"");s=$0;f=1;next}
    NF&&f{ print s"/"$0 }'

cd ../hub

echo ""
echo "----------------------------------------------"
echo "building hub..."
echo $PWD
echo "----------------------------------------------"
echo ""

go build .
echo "seems to have gone well."

echo ""
echo "----------------------------------------------"
echo "building docker image..."
echo "----------------------------------------------"
echo ""

docker build -t qccoders/8ball .

echo ""
echo "done!"
echo ""