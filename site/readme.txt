This file contains information about how to do modification on the website

### Short version ###########################

Change the tutorial (.Rmd) in the general folder and execute the notebook generate_tuto.Rmd. All html file should be regenerated.

### Long version ###########################

The present hierarchy is a bit complex and might seems confusing at first sight. The idea behind this come from a few constraints:

1) The tutorial should be in the based directory, and should be standalone file, i.e. could be used by student to do the tutorial. It means that no html or so should be present in them.
2) The website should be automatically generated from the tutorial, i.e. should reflect exactly the content of the file from the tutorial without having to manually change something. 

The idea is then the following. For each tutorial, we have a template, which contains some html code (very basic) for the nice title and so, and then a title to the tutorial.Rmd file which are used to generate the final page.
Another notebook (generate_tuto.Rmd) is used to render all the template. This allow to regenerate all the website from only one places (we don't need to open every notebook and render them) and add a common menu or whatever more is needed.

Given this idea, the structure is the following:

./generate_tuto.Rmd: is used to generate almost all html pages
./template_*.Rmd contain a link to the page to be render. It define the final title of the page.
../*.Rmd contain the actual tutorial and all the content which is different from one page to another. 
./template_index.Rmd contains the actual content of the first page
./header.html contains some html that has to be put before the actual content, including the menu on the left. 
./generate_plit_design.Rmd allows to regenerate the plot that is on plot of each pages. 
./files/ and ./images/ contain useful files and images for the website. 


The other folder are generated automatically by generate_tuto
