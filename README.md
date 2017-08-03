# r-kriging-servant-client-demo

A demonstration of r-kriging-servant running as multiple instances on one machine.

This is a yesod server that can be started with command line options for ports -r -g -b
where r-kriging-servant servers are expected to be located.

The FreeSketch (P5.js generated display) hosted displays an original image and its RGB channels.

Each RGB channel is sampled randomly (by pressing r,g,b keys respectively to start the process) and the random
sample sent to the corresponding r-kriging-servant server. The kriging servant fits a kriging model and returns
a prediction map which is plotted below. The RGB channels are summed and plotted together underneath the original image.

!(demo)[/screenshots/computerhammer-kriging.jpg]
