Copyright 2009-2012 by Hermann_Elendner

This is the minimal documentation of the official
VGSF LaTeX beamer presentation style. 

This file may be distributed and/or modified
under the GNU Public License, version 3 or later.

See the file docs/LICENSE for more details.

$revision 797

=======================================================

Q: How do I use this?
A: Just open TEMPLATE.tex and adjust/extend it to become your presentation.
Do not change the indicated areas unless you know what you are doing.

-----------------------------

Q: Who will help me?
A: 1. Google, 2. your room mates, 3. the mailing list: If both 1&2 fail after
serious consultation, write up your question clearly and minimally and post
it to the free software mailing list  linux@wu.ac.at .

-----------------------------

Q: What if I want my presentation to look differently?
A: That's not my problem: Convince the managing director that it fulfills the
corporate identity requirements and convince LaTeX to compile it.

-----------------------------

Q: How do I get my suggestions into the general template (for all)?
A: That depends --
   -- on general design: convince the managing director and then me.
   -- special features: provide the code/ adjustments and follow the above.
   -- bug reports: see Q: below.

-----------------------------

Q: What if I desperately need a sidebar/ a top infobar/ some special feature?
A: It's not recommended.  If you insist, the general rule is that you have to
implement it yourself, are welcome to consult the mailing list  linux@wu.ac.at
and are asked to contribute your result once you are done.

-----------------------------

Q: What is a Makefile?  
A: A highly recommended way to automate all common tasks with LaTeX,
e.g. building a project, running calculations, or cleaning up.
See  http://de.wikipedia.org/wiki/Make ,  http://www.gnu.org/software/make/ ,
http://www.steve.org.uk/Software/make/ .

-----------------------------

Q: What are the official VGSF colours and how do I use them?
A: In RGB notation, vgsfblue=4866AC=48,66,172; vgsfyellow=EC9E16=236,158,16.
You can write easily \textcolor{vgsfblue}{in the right blue}.  This works for
vgsfyellow, too, just as for all colours known by the {color} package: red,
green, blue, etc; and any you define (see package documentation).

-----------------------------

Q: Why so many files?  Can I get rid of some?  
A: Every one is needed for something; don't delete any if unsure.

-----------------------------

Q: Can I put them somewhere else?
A: Don't; everything is set up to just work.  Technically the answer is yes,
but it requires knowledge of texmf-trees and texhash, which is up to you to
acquire.  Alternatively (but not recommended), you can put the .sty files in
the same folder where the .tex is and write \usetheme{Vienna} instead of
\usepackage{subfolder/beamerthemeVienna}, and similarly for outer and color
themes.

-----------------------------

Q: Will there be future versions?
A: Certainly.  So /if/ you make local changes, it's your responsibility to
make sure you can re-apply them after updates.

-----------------------------

Q: Do I need to use pdflatex or latex?
A: You can use either.  For beginners, pdflatex is recommended.

-----------------------------

Q: How do I report a bug?
A: Once you've made sure it is a bug, write me an email reporting it in
detail.  Please include a minimal .tex example which compiles.

-----------------------------

Q: Can I share this outside the VGSF?
A: Yes; it is free software (see the license).  Basically, that means that
you are free to use, study, share, and improve it; the condition is that if
you distribute it you must keep it free (provide the source code and keep the
copyleft license intact).

-----------------------------

Q: Where is the website for this stuff?
A: Not up yet.  So far, use the mailing list  linux@wu.ac.at .

-----------------------------

Q: Where do I get more info about beamer?
A: http://www.ctan.org/tex-archive/macros/latex/contrib/beamer/

-----------------------------

Q: Why are the license texts included?
A: To obey the law.

-----------------------------

Q: What if I violate the license?
A: Don't.  The free license gives you every right you can legitimitely ask
for, the only thing you're required to do is to keep attribution and license
intact when you re-destribute it.  You /will/ be sued if you violate this, so
please respect it.

-----------------------------

Q: How do I specify multiple authors, maybe with different institutions?
A: For multiple authors, simply define \author{Alice \and Bob}.  If there are
multiple insitutions, you need to first define 
\author{Alice\inst{1} \and Bob\inst{2} \and Carol\inst{3}}
and then to define in the proper place
\institute[GSG9, MI6 \& VGSF]{\inst{1}GSG9 \and \inst{2}MI6 \and \inst{3}VGSF}
Note that the proper place is *after* \usetheme{Vienna} and *before*
\begin{document}, as an exception to the rule in the comments.
Of course, if Bob defects you need only define two institutes and write
\author{Alice\inst{1} \and Bob\inst{1} \and Carol\inst{2}}.
Alternatively, if you want the footnote to show the institute per author,
define what you want to see there as optional argument to author, and define
the optional argument to institute as empty: 
\author[Alice (GSG9), Bob (MI6), Carol (VGSF)]{Alice\inst{1} \and Bob\inst{2}
  \and Carol\inst{3}} 
as well as
\institute[]{\inst{1}GSG9 \and \inst{2}MI6 \and \inst{3}VGSF}.

-----------------------------

Q: How do I produce backup slides?
A: After the ordinary end of the presentation, put \begin{appendix}, then
your backup slides, then \end{appendix}.  This will produce a thank-you slide
at the end of the regular presentation and define the total number of slides
as this one's.  This prevents confusion caused by a too large slide count and
a presentation end before the highest-numbered slide.  Further slides will
simply appear after the thank you slide, and continue to be numbered.

-----------------------------

Q: Are there any convenience features?
A: Try \st, \nd, \rd, \th, \ie, \eg, \Eg as useful commands, and
\begin{witemize} \item A \item B \end{witemize} for a wide itemize.  Use
\emph{text} and \bold{text} to emphasise.

-----------------------------

Q: How do I avoid text (like the title) in the footline to run over the edge?
A: Define a short title (or short author info) for the footline as optional
argument, eg \title[CAPM]{The Capital Asset Pricing Model}.

-----------------------------

Q: How do I produce agenda slides?
A: Just write \tableofcontents to get the listing of your defined \section's
and \subsections.  Use \tableofcontents[currentsection] to grey out others
than the current section.  (Also note that this works per part: if you have
several \part's, you will get one toc each, ie parts are "meta-sections.")

-----------------------------

Q: How do I merge all overlays onto one slide each?
A: Hand "handout" an option to \documentclass, ie in the square brackets.

-----------------------------


Q: Where do I send questions and suggestions?
A: linux@wu.ac.at

-----------------------------

