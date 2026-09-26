## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----quick-check, eval=FALSE--------------------------------------------------
# library(gpuinfo)
# 
# has_gpu()
# has_cuda()
# has_metal()
# has_rocm()
# has_opencl()
# 
# hardware_info()
# gpu_sitrep()

## ----validation-evidence------------------------------------------------------
library(gpuinfo)
hardware_info()$validation

