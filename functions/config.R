read_config = function(filename = file.path(THESIS_DATA, "config", "v0.yaml")){
  x = read_yaml(filename)
  options(sdmpredictors_datadir = x$sdmpredictors_datadir)
  return(x)
}