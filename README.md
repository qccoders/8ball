# 🎱 Distributed Magic 8-Ball

Ask a yes or no question and get a consensus response from our network of 8-balls! Why settle for just one?!

## Start Web

Assumes [Node.js](https://nodejs.org/en/) and [Yarn](https://yarnpkg.com/en/) are installed.

```
git clone https://github.com/qccoders/8ball
cd 8ball/web
yarn install
yarn start
```

## Start Hub

Assumes [Golang](https://golang.org/doc/install) is installed.

```
git clone https://github.com/qccoders/8ball
cd 8ball/hub
go get -d ./...
go run .
```

## Build Docker Image

Assumes Node.js, Yarn, Golang and Docker are installed.

```
git clone https://github.com/qccoders/8ball
cd 8ball
./build/build.sh
```

## About this App

This app is being collaboratively built by the members of [QC Coders](http://qccoders.org) as part of the [Hacktoberfest 2019](https://hacktoberfest.digitalocean.com/) event.  Our goal is to make something fun while providing our members with experience collaborating on an Open Source project and using Git and GitHub.

We aren't taking this project or ourselves too seriously and you shouldn't either.  We'd like everyone that is interested to participate; if you aren't confident in your skills then this is a great opportunity to learn.

## How to Contribute

Be sure that you've registered on the [Hacktoberfest 2019](https://hacktoberfest.digitalocean.com/) site!

1. Contact us (via Slack, email to info@qccoders.org, or GitHub issue) to be added as a collaborator.
2. Find an open, unassigned issue that you'd like to work on and assign yourself.  If the work you'd like to do doesn't have an issue, create one and wait for feedback from a maintainer before beginning work.
3. Clone (or make sure you're up to date with `git pull`) the repository, then create a new branch for your change with `git checkout -b <your branch name>`.
4. Make your changes.
5. Submit a [Pull Request](https://services.github.com/on-demand/github-cli/open-pull-request-github); be descriptive, and indicate which issue(s) are related to your changes.
6. Your PR must be reviewed and approved by a maintainer and at least one other person prior to being merged. If changes are requested, push additional commits to your branch accordingly.
7. When your PR has been approved it will be merged.  Thanks for your contribution!

If you aren't sure what to do at any point, ask! There are no stupid questions.
