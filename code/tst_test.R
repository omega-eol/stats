# two sample t-test
# Input example
# group 1 - treatment
# m1 = 0.031436784; # sample mean
# s1 = 1.18637505; # sample standard deviation
# n1 = 20838610; # number of samples
# 
# # group 2 - control
# m2 = 0.027056397;
# s2 = 1.056261811;
# n2 = 2323430;
# 
# alpha = 0.05;
# m0 = 0;
tst_test = function(m1, s1, n1, m2, s2, n2, m0=0, alpha=0.05, verbose=TRUE) {

     # standard error of the difference for two independent random samples
     ve = s1*s1/n1 + s2*s2/n2;
     se = sqrt(ve);
     
     # compute degree of freedom
     df = floor(ve/(1/(n1-1)*s1*s1/n1 + 1/(n2-1)*s2*s2/n2));
     
     # find margin of error
     me = qt(alpha/2, df)*se;
     d = m1 - m2;
     
     # confedence interval
     x1 = d - me; x2 = d + me;
     conf_interval = c(min(x1, x2), max(x1, x2));
     
     # find p-value and prepare the res table
     t = (m1-m2-m0)/se; 
     res <- c(d, se, t, 2*pt(-abs(t),df), df); 
     names(res) <- c("Difference of Means", "Std Error", "t", "p-value", "Degree of Freedom");
     
     if (verbose) {
          print(res);
          message("Confedence interval: (", conf_interval[1], ", ", conf_interval[2], ")");
     };
     
     res_l = list("report" = res, "confedence_interval" = conf_interval);
     return(res_l);
};
