sink.reset <- function()
  {
  for(i in seq_len(sink.number()))
    {
    sink(NULL)
  }
}
