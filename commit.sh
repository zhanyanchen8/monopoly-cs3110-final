echo "Enter commit message here: "
read commit_msg
git commit -am commit_msg

echo "pulling\n"
git pull

echo "pushing\n"
git push
