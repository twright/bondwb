#!/usr/bin/env bash

sudo cp ~/.local/bin/bondwb /usr/bin
#echo -e "#"'!'"/usr/bin/env bash\n\ncd $(pwd)\nstack exec bondwb" > /usr/bin/bondwb
sudo chmod a+rx /usr/bin/bondwb
sudo cp ~/.local/bin/cpiwb /usr/bin
#echo -e "#"'!'"/usr/bin/env bash\n\ncd $(pwd)\nstack exec cpiwb" > /usr/bin/cpiwb
sudo chmod a+rx /usr/bin/cpiwb
