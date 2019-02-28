#sudo R CMD javareconf
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/jre/")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH="/Library/Java/JavaVirtualMachines/jdk1.8.0_171.jdk/Contents/Home/jre/lib/server/")

install.packages("RColorBrewer")
library(VennDiagram)
library(RColorBrewer)
grid.newpage()
draw.triple.venn(
  area1 = 24848493,
  area2 = 24392392,
  area3 = 2141156,
  n12 = 15777963,
  n23 = 1527449,
  n13 = 1823157,
  n123 = 1372763,
  category = c('talk', 'story', 'style'),
  fill = RColorBrewer::brewer.pal(3, 'Accent'),
  lty = 'blank',
  cex = rep(1.2, 7),
  cat.cex = rep(1.5, 3),
  alpha = c(0.5, 0.5, 0.5)
)


library(rJava)
library(venneuler)
library(eulerr)
birth_ven = euler(c(Talk=24848493, Story=24392392, Profile=15547058, "Talk&Story"=8977645, "Talk&Profile"=1695381, "Story&Profile"=2248395, "Talk&Story&Profile"=6800318)
                )
plot(birth_ven, key = F, counts = F)


gender_ven = euler(c(Talk=15920185, Story=11227201, Profile=16496058, "Talk&Story"=582633, "Talk&Profile"=1259423, "Story&Profile"=4508131, "Talk&Story&Profile"=4611261)
)
plot(gender_ven, key = F, counts = F)


birth_ven = euler(c(Talk= 21221631 , Story= 3723115 , Profile= 2861178 
                    , "Talk&Story"=3723115
                    , "Talk&Profile"=2861178
                    , "Story&Profile"=2861178
                    , "Talk&Story&Profile"=2861178)
                  )
plot(birth_ven, key = F, counts = F)


