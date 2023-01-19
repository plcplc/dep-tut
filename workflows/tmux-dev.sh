tmux \
  new-session -n dep-tut  \; \
  new-window -t 10 -n 'hoogle server' 'hoogle server --https --cert=ssl/localhost.pem --key=ssl/localhost.key --local' \; \
  select-window -t 0
