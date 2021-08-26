#include <ccp4/cmtzlib.h>
#include <stdio.h>

int main() {
  MTZ* content = MtzGet("../test-data/staraniso_alldata-unique.mtz", 1);
  printf("done");
}
