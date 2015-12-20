# ![physTools logo][logo] physTools
**Command-line tools for simple physics**

The package physTools provides command-line tools for simple physics, written in
Fortran. The package is currently in the alpha stage; a number of programs sit around
on my computer, waiting to be gathered, tidied up and added &mdash; stay tuned (December
2015).

The default installer uses [CMake](https://cmake.org/). The code has been used
with [gfortran](https://gcc.gnu.org/fortran/) (4.9 or newer),
[g95](http://www.g95.org) and
[Intel Fortran](http://software.intel.com/en-us/articles/non-commercial-software-development/)
and can be used under the conditions of version 3 of the
[GPL](http://www.gnu.org/licenses/gpl.html). PhysTools is developed by
[AstroFloyd](http://astrofloyd.org/).



## Installation
In order to compile and install physTools, you'll need a
[Fortran compiler](https://gcc.gnu.org/fortran/), [CMake](https://cmake.org/) and
[libSUFR](http://libsufr.sourceforge.net) installed.
From the package/git root directory (probably named physTools/ and containing
CMakeLists.txt), do e.g.:

1. mkdir build && cd build/
2. cmake ..
3. make
4. sudo make install

For more compilation and installation options, see the [INSTALL](doc/INSTALL) file.


## Similar packages
If you like physTools, you may be interested in the following FOSS packages as well:

* [libSUFR](http://libsufr.sourceforge.net):         a **lib**rary containing **S**ome **U**seful **F**ortran **R**outines
* [astroTools](http://astrotools.sourceforge.net):   command-line tools for astronomy and astrophysics
* [statTools](http://stattools.sourceforge.net):     command-line tools for simple statistics
* [GWtool](http://astrotools.sourceforge.net):       command-line tools for gravitational waves



## Contact
You can contact AstroFloyd through [AstroFloyd.org](http://astrofloyd.org) or
send a pull request through [GitHub](https://github.com/AstroFloyd).


<!-- Start of StatCounter Code for AF Code -->
<script type="text/javascript">
  var sc_project=7293559; 
  var sc_invisible=1; 
  var sc_security="9a0c50a0"; 
</script>
<script type="text/javascript" src="http://www.statcounter.com/counter/counter.js">
</script>
<noscript>
  <div class="statcounter">
<a title="counter on godaddy" href="http://statcounter.com/godaddy_website_tonight/">
  <img class="statcounter" src="http://c.statcounter.com/7293559/0/9a0c50a0/1/" alt="counter on godaddy">
</a>
  </div>
</noscript>
<!-- End of StatCounter Code for AF Code -->


<br>
<section itemscope="" itemtype="http://data-vocabulary.org/Person" style="text-align:center; color:#888888; font-size:65%; margin:0;">
  &copy; 2009–2015 &nbsp; 
  <a href="https://plus.google.com/107239694113064141819?rel=author" itemprop="name" style="color:#888888;">by Astro Floyd</a>, 
  <a href="http://astrofloyd.org" title="Contact" style="color:#888888;" span="" itemprop="organization">astrofloyd.org</a>  &nbsp;–&nbsp; 
  <span itemprop="location">Planet Earth</span>
</section>

[logo]: https://raw.githubusercontent.com/AstroFloyd/physTools/master/doc/pT_sm.png "physTools logo"
