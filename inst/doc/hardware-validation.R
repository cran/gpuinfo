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

## ----host-resources, eval=FALSE-----------------------------------------------
# cpu_info()
# system_memory_info()
# hardware_info()$gpu_summary

## ----benchmark-memory, eval=FALSE---------------------------------------------
# device_memory <- gpu_memory_info()
# before <- gpu_memory_sample()
# 
# # Run GPU work here. Synchronize with the framework before sampling if the
# # operation is asynchronous.
# 
# after <- gpu_memory_sample()
# manifest <- hardware_manifest(memory = "process")
# 
# # For worker-based benchmarks, collect several snapshots and aggregate them.
# devices <- subset(gpu_devices(), process_visible)
# peak <- gpu_memory_peak(list(before, after), devices)

## ----validation-evidence------------------------------------------------------
library(gpuinfo)
hardware_info()$validation

