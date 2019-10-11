cd web

echo "building web"
pwd

npm run build

cp -r build/. ../hub/public

cd ../hub
echo "building hub"
pwd 

go build .

echo "building docker image"

docker build -t qccoders/8ball .

echo "done!"