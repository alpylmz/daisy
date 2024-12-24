#include <bits/stdc++.h>

typedef struct {
  float low;
  float high;
} innnterval;

innnterval ifelsefunc(float xlo, float xhi, float ylo, float yhi) {
    //float xlo = x.low;
    //float xhi = x.high;
    //float ylo = y.low;
    //float yhi = y.high;
    std::cout << "C called!" << std::endl;
    std::cout << "xlo: " << xlo << std::endl;
    std::cout << "xhi: " << xhi << std::endl;
    std::cout << "ylo: " << ylo << std::endl;
    std::cout << "yhi: " << yhi << std::endl;
    if (xlo >= 0) {
      if (ylo >= 0.0) {
        return innnterval{xlo * ylo, xhi * yhi};
      } else if (yhi <= 0.0) {
        return innnterval{xhi * ylo, xlo * yhi};
      } else {
        return innnterval{xhi * ylo, xhi * yhi};
      }
    }
    else if (xhi > 0.0) {
      if (ylo >= 0.0) {
        return innnterval{xlo * yhi, xhi * yhi};
      } else if (yhi <= 0.0) {
        return innnterval{xhi * ylo, xlo * ylo};
      } else {
        float a = std::min(xlo * yhi, xhi * ylo);
        float b = std::max(xlo * ylo, xhi * yhi);
        return innnterval{a, b};
      }
    }
    else {
      if (ylo >= 0.0) {
        return innnterval{xlo * yhi, xhi * ylo};
      } else if (yhi <= 0.0) {
        return innnterval{xhi * yhi, xlo * ylo};
      } else {
        return innnterval{xlo * yhi, xlo * ylo};
      }
    }
}

// write the ifelsefunc so that it will return the lower bound only
float ifelsefunclower(float xlo, float xhi, float ylo, float yhi){
    if(xlo >= 0){
        if(ylo >= 0){
            return xlo * ylo;
        } else if(yhi <= 0){
            return xhi * ylo;
        } else {
            return xhi * ylo;
        }
    } else if(xhi > 0){
        if(ylo >= 0){
            return xlo * yhi;
        } else if(yhi <= 0){
            return xhi * ylo;
        } else {
            return std::min(xlo * yhi, xhi * ylo);
        }
    } else {
        if(ylo >= 0){
            return xlo * yhi;
        } else if(yhi <= 0){
            return xhi * yhi;
        } else {
            return xlo * yhi;
        }
    }
}

// write the ifelsefunc so that it will return the upper bound only
float ifelsefuncupper(float xlo, float xhi, float ylo, float yhi){
    if(xlo >= 0){
        if(ylo >= 0){
            return xhi * yhi;
        } else if(yhi <= 0){
            return xlo * yhi;
        } else {
            return xhi * yhi;
        }
    } else if(xhi > 0){
        if(ylo >= 0){
            return xhi * yhi;
        } else if(yhi <= 0){
            return xlo * ylo;
        } else {
            return std::max(xlo * ylo, xhi * yhi);
        }
    } else {
        if(ylo >= 0){
            return xhi * ylo;
        } else if(yhi <= 0){
            return xlo * ylo;
        } else {
            return xlo * ylo;
        }
    }
}

float wholemultlower(
    float abstractRangeLhsxlo, float abstractRangeLhsxhi,
    float abstractRangeRhsxlo, float abstractRangeRhsxhi,
    float errorRhsxlo, float errorRhsxhi,
    float errorLhsxlo, float errorLhsxhi){
        // the func is
        // err1 = abstractRangeLhs * errorRhs
        // err2 = abstractRangeRhs * errorLhs
        // err3 = err1 * err2
        // err = (
        //    err1.xlo + err2.xlo + err3.xlo,
        //    err1.xhi + err2.xhi + err3.xhi
        //)
        float err1xlo = ifelsefunclower(abstractRangeLhsxlo, abstractRangeLhsxhi, errorRhsxlo, errorRhsxhi);
        //float err1xhi = ifelsefuncupper(abstractRangeLhsxlo, abstractRangeLhsxhi, errorRhsxlo, errorRhsxhi);
        float err2xlo = ifelsefunclower(abstractRangeRhsxlo, abstractRangeRhsxhi, errorLhsxlo, errorLhsxhi);
        //float err2xhi = ifelsefuncupper(abstractRangeRhsxlo, abstractRangeRhsxhi, errorLhsxlo, errorLhsxhi);
        float err3xlo = ifelsefunclower(errorLhsxlo, errorLhsxhi, errorRhsxlo, errorRhsxhi);
        //float err3xhi = ifelsefuncupper(errorLhsxlo, errorLhsxhi, errorRhsxlo, errorRhsxhi);
        float errxlo = err1xlo + err2xlo + err3xlo;
        //float errxhi = err1xhi + err2xhi + err3xhi;
        //std::cout << "err1xlo: " << err1xlo << std::endl;
        //std::cout << "err1xhi: " << err1xhi << std::endl;
        //std::cout << "err2xlo: " << err2xlo << std::endl;
        //std::cout << "err2xhi: " << err2xhi << std::endl;
        //std::cout << "err3xlo: " << err3xlo << std::endl;
        //std::cout << "err3xhi: " << err3xhi << std::endl;
        //std::cout << "upper_res_holder: " << upper_res_holder << std::endl;
        //std::cout << "returning: " << errxlo << std::endl;
        return errxlo;
    }


float wholemultupper(
    float abstractRangeLhsxlo, float abstractRangeLhsxhi,
    float abstractRangeRhsxlo, float abstractRangeRhsxhi,
    float errorRhsxlo, float errorRhsxhi,
    float errorLhsxlo, float errorLhsxhi){
    // the func is
        // err1 = abstractRangeLhs * errorRhs
        // err2 = abstractRangeRhs * errorLhs
        // err3 = err1 * err2
        // err = (
        //    err1.xlo + err2.xlo + err3.xlo,
        //    err1.xhi + err2.xhi + err3.xhi
        //)
        //float err1xlo = ifelsefunclower(abstractRangeLhsxlo, abstractRangeLhsxhi, errorRhsxlo, errorRhsxhi);
        float err1xhi = ifelsefuncupper(abstractRangeLhsxlo, abstractRangeLhsxhi, errorRhsxlo, errorRhsxhi);
        //float err2xlo = ifelsefunclower(abstractRangeRhsxlo, abstractRangeRhsxhi, errorLhsxlo, errorLhsxhi);
        float err2xhi = ifelsefuncupper(abstractRangeRhsxlo, abstractRangeRhsxhi, errorLhsxlo, errorLhsxhi);
        //float err3xlo = ifelsefunclower(errorLhsxlo, errorLhsxhi, errorRhsxlo, errorRhsxhi);
        float err3xhi = ifelsefuncupper(errorLhsxlo, errorLhsxhi, errorRhsxlo, errorRhsxhi);
        //float errxlo = err1xlo + err2xlo + err3xlo;
        float errxhi = err1xhi + err2xhi + err3xhi;
        //std::cout << "err1xlo: " << err1xlo << std::endl;
        //std::cout << "err1xhi: " << err1xhi << std::endl;
        //std::cout << "err2xlo: " << err2xlo << std::endl;
        //std::cout << "err2xhi: " << err2xhi << std::endl;
        //std::cout << "err3xlo: " << err3xlo << std::endl;
        //std::cout << "err3xhi: " << err3xhi << std::endl;
        //std::cout << "upper_res_holder: " << upper_res_holder << std::endl;
        //std::cout << "returning: " << errxlo << std::endl;
        return errxhi;
}
