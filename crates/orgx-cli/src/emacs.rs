use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use sprite_direct::{eval_blocking, quote, Sexp, SpriteError};

#[derive(Debug, Clone)]
pub struct EmacsClient {
    pub target: String,
}

impl EmacsClient {
    pub fn new(explicit_target: Option<&str>) -> Result<Self, String> {
        let target = resolve_target_string(explicit_target)?;
        Ok(Self { target })
    }

    pub fn eval(&self, form: &Sexp) -> Result<String, SpriteError> {
        eval_blocking(&self.target, form, None, Some(Duration::from_secs(5)))
    }

    pub fn ping(&self) -> Result<String, String> {
        let form = Sexp::List(vec![
            Sexp::Sym("format".to_string()),
            Sexp::Str("Emacs %s connected".to_string()),
            Sexp::Sym("emacs-version".to_string()),
        ]);
        self.eval(&form).map_err(|e| format!("Emacs eval error: {}", e))
    }

    pub fn resolve_default_target(&self, dir: &str) -> Result<(String, String), String> {
        let form = Sexp::List(vec![
            Sexp::Sym("progn".to_string()),
            Sexp::List(vec![
                Sexp::Sym("require".to_string()),
                quote(Sexp::Sym("orgx-capture-shell".to_string())),
            ]),
            Sexp::List(vec![
                Sexp::Sym("let".to_string()),
            Sexp::List(vec![
                Sexp::List(vec![
                    Sexp::Sym("res".to_string()),
                    Sexp::List(vec![
                        Sexp::Sym("orgx-capture-shell--resolve-target".to_string()),
                        Sexp::Str(dir.to_string()),
                    ]),
                ]),
            ]),
            Sexp::List(vec![
                Sexp::Sym("format".to_string()),
                Sexp::Str("%s|%s".to_string()),
                Sexp::List(vec![Sexp::Sym("car".to_string()), Sexp::Sym("res".to_string())]),
                Sexp::List(vec![Sexp::Sym("cdr".to_string()), Sexp::Sym("res".to_string())]),
            ]),
            ]),
        ]);

        let raw = self.eval(&form).map_err(|e| format!("Eval error: {}", e))?;
        let clean = raw.trim().trim_matches('"');
        let parts: Vec<&str> = clean.splitn(2, '|').collect();
        if parts.len() == 2 {
            Ok((parts[0].to_string(), parts[1].to_string()))
        } else {
            Ok((clean.to_string(), "Tasks".to_string()))
        }
    }

    pub fn capture(
        &self,
        file: &str,
        headline: &str,
        template: &str,
        text: &str,
    ) -> Result<String, String> {
        let form = Sexp::List(vec![
            Sexp::Sym("progn".to_string()),
            Sexp::List(vec![
                Sexp::Sym("require".to_string()),
                quote(Sexp::Sym("orgx-capture-shell".to_string())),
            ]),
            Sexp::List(vec![
                Sexp::Sym("let*".to_string()),
            Sexp::List(vec![
                Sexp::List(vec![
                    Sexp::Sym("f".to_string()),
                    Sexp::List(vec![
                        Sexp::Sym("expand-file-name".to_string()),
                        Sexp::Str(file.to_string()),
                    ]),
                ]),
                Sexp::List(vec![
                    Sexp::Sym("h".to_string()),
                    if headline.is_empty() {
                        Sexp::Sym("nil".to_string())
                    } else {
                        Sexp::Str(headline.to_string())
                    },
                ]),
                Sexp::List(vec![
                    Sexp::Sym("tpl".to_string()),
                    quote(Sexp::Sym(template.to_string())),
                ]),
                Sexp::List(vec![
                    Sexp::Sym("entry".to_string()),
                    Sexp::List(vec![
                        Sexp::Sym("orgx-capture-shell--format-entry".to_string()),
                        Sexp::Str(text.to_string()),
                        Sexp::Sym("tpl".to_string()),
                    ]),
                ]),
                Sexp::List(vec![
                    Sexp::Sym("rec".to_string()),
                    Sexp::List(vec![
                        Sexp::Sym("orgx-capture-shell--insert-entry".to_string()),
                        Sexp::Sym("f".to_string()),
                        Sexp::Sym("h".to_string()),
                        Sexp::Sym("entry".to_string()),
                    ]),
                ]),
            ]),
            Sexp::List(vec![
                Sexp::Sym("format".to_string()),
                Sexp::Str("Captured -> [%s%s] %s".to_string()),
                Sexp::List(vec![
                    Sexp::Sym("file-name-nondirectory".to_string()),
                    Sexp::Sym("f".to_string()),
                ]),
                Sexp::List(vec![
                    Sexp::Sym("if".to_string()),
                    Sexp::Sym("h".to_string()),
                    Sexp::List(vec![
                        Sexp::Sym("format".to_string()),
                        Sexp::Str(":%s".to_string()),
                        Sexp::Sym("h".to_string()),
                    ]),
                    Sexp::Str("".to_string()),
                ]),
                Sexp::Sym("entry".to_string()),
            ]),
            ]),
        ]);

        let res = self.eval(&form).map_err(|e| format!("Capture error: {}", e))?;
        Ok(res.trim().trim_matches('"').to_string())
    }

    pub fn undo(&self) -> Result<String, String> {
        let form = Sexp::List(vec![
            Sexp::Sym("progn".to_string()),
            Sexp::List(vec![
                Sexp::Sym("require".to_string()),
                quote(Sexp::Sym("orgx-capture-shell".to_string())),
            ]),
            Sexp::List(vec![
                Sexp::Sym("orgx-capture-shell--undo-last".to_string()),
            ]),
        ]);
        let res = self.eval(&form).map_err(|e| format!("Undo error: {}", e))?;
        Ok(res.trim().trim_matches('"').to_string())
    }
}

fn resolve_target_string(explicit: Option<&str>) -> Result<String, String> {
    if let Some(t) = explicit {
        if Path::new(t).is_file() {
            return read_server_file(t);
        }
        return Ok(t.to_string());
    }

    if let Ok(env_path) = std::env::var("EMACS_SERVER_FILE") {
        if Path::new(&env_path).is_file() {
            return read_server_file(&env_path);
        }
    }

    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    let server_dir = PathBuf::from(home).join(".emacs.d/server");
    for candidate in &["tychoish", "server", "work", "hud"] {
        let p = server_dir.join(candidate);
        if p.is_file() {
            if let Ok(t) = read_server_file(p.to_str().unwrap()) {
                return Ok(t);
            }
        }
    }

    // Try default unix socket paths
    extern "C" { fn geteuid() -> u32; }
    let uid = unsafe { geteuid() };
    let default_socket = format!("/run/user/{}/emacs/server", uid);
    if Path::new(&default_socket).exists() {
        return Ok(default_socket);
    }

    Err("Could not locate active Emacs server. Is Emacs running with server-start?".to_string())
}

fn read_server_file(path: &str) -> Result<String, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read Emacs server file {}: {}", path, e))?;
    let lines: Vec<&str> = content.lines().collect();
    if lines.len() >= 2 {
        let host_port = lines[0].split_whitespace().next().unwrap_or(lines[0]);
        let auth_key = lines[1].trim();
        Ok(format!("{}:{}", host_port, auth_key))
    } else if lines.len() == 1 {
        Ok(lines[0].to_string())
    } else {
        Err(format!("Empty server file {}", path))
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn parses_server_file_content() {
        let content = "127.0.0.1:43171 808\nUv<Q'Su7<!*7z~[qk'r_;r.hX4gSX1umT#Qd\"\"LPD0uG!PFcOcl8O5il;S\\M/+Eq\n";
        let lines: Vec<&str> = content.lines().collect();
        let host_port = lines[0].split_whitespace().next().unwrap();
        let key = lines[1].trim();
        assert_eq!(format!("{}:{}", host_port, key), "127.0.0.1:43171:Uv<Q'Su7<!*7z~[qk'r_;r.hX4gSX1umT#Qd\"\"LPD0uG!PFcOcl8O5il;S\\M/+Eq");
    }
}
