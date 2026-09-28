# ==============================================================================
# MOTOR DE INTEROPERABILIDAD GEOMÁTICA - VERSIÓN PRO (FINAL)
# ==============================================================================

.j_render_output <- function(res_raw, max_width = 90) {
  fmt_html <- FALSE; fmt_pdf <- FALSE
  if ("knitr" %in% loadedNamespaces()) {
    fmt_html <- knitr::is_html_output()
    fmt_pdf <- knitr::is_latex_output()
  }

  safe_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    return(x)
  }

  safe_escape_latex <- function(x) {
    if (is.null(x) || length(x) == 0 || x == "") return("")
    chars <- strsplit(x, "")[[1]]
    res <- sapply(chars, function(c) {
      if (c == "\\") return("\\textbackslash{}")
      if (c == "{") return("\\{")
      if (c == "}") return("\\}")
      if (c == "$") return("\\$")
      if (c == "&") return("\\&")
      if (c == "%") return("\\%")
      if (c == "#") return("\\#")
      if (c == "_") return("\\_")
      if (c == "^") return("\\textasciicircum{}")
      if (c == "~") return("\\textasciitilde{}")
      if (c == "<") return("\\textless{}")
      if (c == ">") return("\\textgreater{}")
      if (c == " ") return("~") 
      return(c)
    })
    return(paste(res, collapse = ""))
  }

  wrap_line <- function(line, width) {
    if (nchar(line) == 0) return("")
    
    match_spaces <- regexpr("^ +", line)
    leading_spaces <- regmatches(line, match_spaces)
    if (length(leading_spaces) == 0) leading_spaces <- ""
    
    indent_len <- nchar(leading_spaces)
    
    parts <- c()
    current_str <- line
    
    if (nchar(current_str) > width) {
      parts <- c(parts, substr(current_str, 1, width))
      current_str <- substr(current_str, width + 1, nchar(current_str))
    } else {
      return(line)
    }
    
    wrap_width <- width - indent_len
    if (wrap_width < 10) wrap_width <- 10
    
    while (nchar(current_str) > 0) {
      if (nchar(current_str) > wrap_width) {
        parts <- c(parts, paste0(leading_spaces, substr(current_str, 1, wrap_width)))
        current_str <- substr(current_str, wrap_width + 1, nchar(current_str))
      } else {
        parts <- c(parts, paste0(leading_spaces, current_str))
        current_str <- ""
      }
    }
    
    return(paste(parts, collapse = "\n"))
  }

  # 1. Purga global de artefactos de formato
  res_raw <- gsub("\r", "", res_raw)
  res_raw <- gsub("\u00A0", " ", res_raw)
  res_raw <- gsub("\t", "    ", res_raw)
  
  res_l <- gsub("\033\\[[0-9;]*m", "", res_raw)
  partes <- strsplit(res_l, "\n")[[1]]
  
  out_triple <- 0; out_comentario <- 0; out_comillas <- 0; out_bloque <- 0
  current_mode <- "none" 

  for (p in partes) {
    is_prompt <- grepl("^julia> ", p)
    is_struct_code <- is_prompt || (out_triple > 0) || (out_comentario > 0) || (out_bloque > 0) || (out_comillas > 0)
    
    if (trimws(p) == "" && current_mode != "none") {
      is_code <- (current_mode == "code")
    } else {
      is_code <- is_struct_code
    }
    
    if (is_code) {
      # Conservación matemática de la sangría original sin sustracciones erróneas
      if (is_prompt) {
        p_clean <- sub("^julia> ", "", p)
      } else {
        p_clean <- p 
      }
      p_ev <- p_clean
      
      p_ev <- gsub('""".*?"""', '""', p_ev)
      if (out_triple == 0 && grepl('"""', p_ev)) {
          p_ev <- sub('""".*', '""', p_ev)
      } else if (out_triple == 1 && grepl('"""', p_ev)) {
          p_ev <- sub('.*"""', '""', p_ev)
      } else if (out_triple == 1) {
          p_ev <- "" 
      }
      n_triples <- lengths(regmatches(p_clean, gregexpr('"""', p_clean, fixed = TRUE)))
      out_triple <- (out_triple + n_triples) %% 2
      
      p_ev <- gsub('"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"', '""', p_ev, perl = TRUE)
      p_ev <- gsub("'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'", "''", p_ev, perl = TRUE)
      
      n_abre <- lengths(regmatches(p_ev, gregexpr("#=", p_ev, fixed = TRUE)))
      n_cierra <- lengths(regmatches(p_ev, gregexpr("=#", p_ev, fixed = TRUE)))
      out_comentario <- out_comentario + n_abre - n_cierra
      
      if (out_triple == 0 && out_comentario <= 0 && p_ev != "") {
        p_ev <- sub("#.*", "", p_ev) 
        if (p_ev != "") {
          patron_abrir <- "\\b(do|function|begin|let|while|macro|try|module|baremodule|struct|quote)\\b"
          patron_for_if <- "(^|[=;,\\[\\(\\{]|\\b(?:begin|else|return)\\b)\\s*\\b(for|if)\\b"
          
          abrir_out <- lengths(regmatches(p_ev, gregexpr(patron_abrir, p_ev))) + 
                       lengths(regmatches(p_ev, gregexpr(patron_for_if, p_ev, perl = TRUE))) +
                       lengths(regmatches(p_ev, gregexpr("(", p_ev, fixed = TRUE))) + 
                       lengths(regmatches(p_ev, gregexpr("[", p_ev, fixed = TRUE)))
          
          cerrar_out <- lengths(regmatches(p_ev, gregexpr("\\bend\\b", p_ev))) + 
                        lengths(regmatches(p_ev, gregexpr(")", p_ev, fixed = TRUE))) + 
                        lengths(regmatches(p_ev, gregexpr("]", p_ev, fixed = TRUE)))
                        
          out_bloque <- out_bloque + abrir_out - cerrar_out
        }
      }
      
      if (fmt_html) {
        if (current_mode != "code") {
          if (current_mode != "none") cat("</pre>\n")
          cat("<pre class='julia-code' style='display:block; background:#f4f6f8; border:1px solid #c0c8d0; border-radius:4px; padding:10px; margin-top:10px; margin-bottom:0; font-family:\"Lucida Console\", Consolas, monospace; font-size:0.9em; white-space:pre-wrap; color:#333;'>\n")
          current_mode <- "code"
        }
        
        if (is_prompt) cat("<span style='color:#28a745; font-weight:bold;'>julia&gt;</span>\n")
        
        p_print <- wrap_line(p_clean, max_width)
        if (nchar(p_print) > 0 || !is_prompt) {
          cat(paste0("<span style='color:#008b8b;'>", safe_escape(p_print), "</span>\n"))
        }
        
      } else if (!fmt_pdf) {
        if (is_prompt) cat("\033[32mjulia>\033[0m\n")
        
        p_print <- wrap_line(p_clean, max_width)
        if (nchar(p_print) > 0 || !is_prompt) {
          cat(paste0("\033[36m", p_print, "\033[0m\n"))
        }
        
      } else { 
        if (current_mode != "code") {
          if (current_mode != "none") cat("\\endgroup\n")
          cat("\\begingroup\n\\small\n\\ttfamily\n\\raggedright\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{0pt}\n")
          current_mode <- "code"
        }
        
        if (is_prompt) cat("\\noindent\\textcolor[RGB]{40,167,69}{\\textbf{julia>}}\\par\n")
        
        p_print <- wrap_line(p_clean, max_width)
        if (nchar(p_print) > 0 || !is_prompt) {
          lineas_pdf <- strsplit(p_print, "\n")[[1]]
          if (length(lineas_pdf) == 0) lineas_pdf <- ""
          
          for (l_pdf in lineas_pdf) {
            p_esc <- safe_escape_latex(l_pdf)
            if (p_esc == "") p_esc <- "\\strut"
            cat(paste0("\\noindent\\textcolor[RGB]{0,139,139}{", p_esc, "}\\par\n"))
          }
        }
      }
      
    } else {
      p_print <- wrap_line(p, max_width)
      
      if (!grepl("  ", p_print) && !grepl("│", p_print) && !grepl("×", p_print) && !grepl("^\\[", p_print)) {
        p_print <- sub("^ +", "", p_print)
      }
      
      if (fmt_html) {
        if (current_mode != "data") {
          if (current_mode != "none") cat("</pre>\n")
          cat("<pre class='julia-output' style='display:block; background:#ffffff; border:none; padding:10px 10px 10px 20px; margin-top:0; margin-bottom:15px; font-family:\"Lucida Console\", Consolas, monospace; font-size:0.9em; white-space:pre-wrap; color:#000;'>\n")
          current_mode <- "data"
        }
        cat(paste0(safe_escape(p_print), "\n"))
        
      } else if (!fmt_pdf) { 
        cat(paste0(p_print, "\n")) 
        
      } else {
        if (current_mode != "data") {
          if (current_mode != "none") cat("\\endgroup\n")
          cat("\\begingroup\n\\small\n\\ttfamily\n\\raggedright\n\\setlength{\\parindent}{0pt}\n\\setlength{\\parskip}{0pt}\n")
          current_mode <- "data"
        }
        
        lineas_pdf <- strsplit(p_print, "\n")[[1]]
        if (length(lineas_pdf) == 0) lineas_pdf <- ""
        
        for (l_pdf in lineas_pdf) {
          p_esc <- safe_escape_latex(l_pdf)
          if (p_esc == "") p_esc <- "\\strut"
          cat(paste0("\\noindent\\textcolor[RGB]{51,51,51}{", p_esc, "}\\par\n"))
        }
      }
    }
  }
  if (fmt_html && current_mode != "none") {
    cat("</pre>\n")
  } else if (fmt_pdf && current_mode != "none") {
    cat("\\endgroup\n\\vspace{0.05cm}\n")
  }
}

j_eval <- function(cmd, max_width = 90) {
  .ensure_julia_ready()
  
  cmd <- gsub("\r", "", cmd)
  cmd <- gsub("\u00A0", " ", cmd)
  cmd <- gsub("\t", "    ", cmd)
  
  lineas <- strsplit(cmd, "\n")[[1]]
  buffer <- ""; en_bloque <- 0; en_comentario_multi <- 0; en_triple_comilla <- 0
  resultado_final <- NULL

  for (l in lineas) {
    if (trimws(l) == "" && en_comentario_multi == 0 && en_triple_comilla == 0 && en_bloque == 0) next
    buffer <- paste0(buffer, l, "\n")
    l_clean <- l
    
    l_clean <- gsub('""".*?"""', '""', l_clean)
    if (en_triple_comilla == 0 && grepl('"""', l_clean)) {
        l_clean <- sub('""".*', '""', l_clean)
    } else if (en_triple_comilla == 1 && grepl('"""', l_clean)) {
        l_clean <- sub('.*"""', '""', l_clean)
    } else if (en_triple_comilla == 1) {
        l_clean <- ""
    }
    num_triples <- lengths(regmatches(l, gregexpr('"""', l, fixed = TRUE)))
    en_triple_comilla <- (en_triple_comilla + num_triples) %% 2
    
    if (en_triple_comilla == 0 && l_clean != "") {
      l_clean <- gsub('"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"', '""', l_clean, perl = TRUE)
      l_clean <- gsub("'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'", "''", l_clean, perl = TRUE)
      
      num_com_abre <- lengths(regmatches(l_clean, gregexpr("#=", l_clean, fixed = TRUE)))
      num_com_cierra <- lengths(regmatches(l_clean, gregexpr("=#", l_clean, fixed = TRUE)))
      en_comentario_multi <- en_comentario_multi + num_com_abre - num_com_cierra
      
      if (en_comentario_multi <= 0) {
        l_clean <- sub("#.*", "", l_clean)
        
        if (trimws(l_clean) != "") {
          patron_abrir <- "\\b(do|function|begin|let|while|macro|try|module|baremodule|struct|quote)\\b"
          patron_for_if <- "(^|[=;,\\[\\(\\{]|\\b(?:begin|else|return)\\b)\\s*\\b(for|if)\\b"
          
          abrir <- lengths(regmatches(l_clean, gregexpr(patron_abrir, l_clean))) + 
                   lengths(regmatches(l_clean, gregexpr(patron_for_if, l_clean, perl = TRUE))) +
                   lengths(regmatches(l_clean, gregexpr("(", l_clean, fixed = TRUE))) + 
                   lengths(regmatches(l_clean, gregexpr("[", l_clean, fixed = TRUE)))
                   
          cerrar <- lengths(regmatches(l_clean, gregexpr("\\bend\\b", l_clean))) + 
                    lengths(regmatches(l_clean, gregexpr(")", l_clean, fixed = TRUE))) + 
                    lengths(regmatches(l_clean, gregexpr("]", l_clean, fixed = TRUE)))
                    
          en_bloque <- en_bloque + abrir - cerrar
        }
      }
    }
    
    if (en_bloque <= 0 && en_comentario_multi <= 0 && en_triple_comilla == 0) {
      res_raw <- JuliaConnectoR::juliaCall("_unal_core_executor", buffer, FALSE, "", 72, 800, 500, 12)
      .j_render_output(res_raw, max_width)
      
      res_l <- gsub("\033\\[[0-9;]*m", "", res_raw)
      lineas_res <- strsplit(res_l, "\n")[[1]]
      lineas_res <- trimws(lineas_res[lineas_res != ""])
      temp_res <- tail(lineas_res[!grepl("^julia>", lineas_res)], 1)
      if (length(temp_res) > 0) resultado_final <- temp_res
      
      buffer <- ""; en_bloque <- 0; en_comentario_multi <- 0; en_triple_comilla <- 0
    }
  }
  return(invisible(resultado_final))
}

j_plot <- function(cmd, n = "tmp_plot.png", dpi = 300, w = 800, h = NULL, ratio = 1.6, fontsize = 12, max_width = 90) {
  .ensure_julia_ready()
  if (is.null(h)) h <- round(w / ratio)
  
  cmd <- gsub("\r", "", cmd)
  cmd <- gsub("\u00A0", " ", cmd)
  cmd <- gsub("\t", "    ", cmd)
  
  lineas <- strsplit(cmd, "\n")[[1]]
  buffer <- ""; en_bloque <- 0; en_comentario_multi <- 0; en_triple_comilla <- 0
  
  total_lineas <- length(lineas)
  
  for (i in seq_along(lineas)) {
    l <- lineas[i]
    if (trimws(l) == "" && en_comentario_multi == 0 && en_triple_comilla == 0 && en_bloque == 0) next
    
    buffer <- paste0(buffer, l, "\n")
    l_clean <- l
    
    l_clean <- gsub('""".*?"""', '""', l_clean)
    if (en_triple_comilla == 0 && grepl('"""', l_clean)) {
        l_clean <- sub('""".*', '""', l_clean)
    } else if (en_triple_comilla == 1 && grepl('"""', l_clean)) {
        l_clean <- sub('.*"""', '""', l_clean)
    } else if (en_triple_comilla == 1) {
        l_clean <- ""
    }
    num_triples <- lengths(regmatches(l, gregexpr('"""', l, fixed = TRUE)))
    en_triple_comilla <- (en_triple_comilla + num_triples) %% 2
    
    if (en_triple_comilla == 0 && l_clean != "") {
      l_clean <- gsub('"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"', '""', l_clean, perl = TRUE)
      l_clean <- gsub("'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'", "''", l_clean, perl = TRUE)
      
      num_com_abre <- lengths(regmatches(l_clean, gregexpr("#=", l_clean, fixed = TRUE)))
      num_com_cierra <- lengths(regmatches(l_clean, gregexpr("=#", l_clean, fixed = TRUE)))
      en_comentario_multi <- en_comentario_multi + num_com_abre - num_com_cierra

      if (en_comentario_multi <= 0) {
        l_clean <- sub("#.*", "", l_clean)
        
        if (trimws(l_clean) != "") {
          patron_abrir <- "\\b(do|function|begin|let|while|macro|try|module|baremodule|struct|quote)\\b"
          patron_for_if <- "(^|[=;,\\[\\(\\{]|\\b(?:begin|else|return)\\b)\\s*\\b(for|if)\\b"
          
          abrir <- lengths(regmatches(l_clean, gregexpr(patron_abrir, l_clean))) + 
                   lengths(regmatches(l_clean, gregexpr(patron_for_if, l_clean, perl = TRUE))) +
                   lengths(regmatches(l_clean, gregexpr("(", l_clean, fixed = TRUE))) + 
                   lengths(regmatches(l_clean, gregexpr("[", l_clean, fixed = TRUE)))
                   
          cerrar <- lengths(regmatches(l_clean, gregexpr("\\bend\\b", l_clean))) + 
                    lengths(regmatches(l_clean, gregexpr(")", l_clean, fixed = TRUE))) + 
                    lengths(regmatches(l_clean, gregexpr("]", l_clean, fixed = TRUE)))
                    
          en_bloque <- en_bloque + abrir - cerrar
        }
      }
    }
    
    if (en_bloque <= 0 && en_comentario_multi <= 0 && en_triple_comilla == 0) {
      
      es_ultimo_bloque <- (i == total_lineas) || all(trimws(lineas[(i+1):total_lineas]) == "")
      
      res_log <- JuliaConnectoR::juliaCall("_unal_core_executor", buffer, es_ultimo_bloque, n, dpi, as.integer(w), as.integer(h), as.integer(fontsize))
      .j_render_output(res_log, max_width)
      buffer <- ""; en_bloque <- 0; en_comentario_multi <- 0; en_triple_comilla <- 0
    }
  }
  
  if (file.exists(n)) {
    img <- png::readPNG(n)
    grid::grid.newpage(); grid::grid.raster(img)
  }
}

