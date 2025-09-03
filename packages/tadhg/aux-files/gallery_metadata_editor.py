import os
import sys
from flask import Flask, request, redirect, url_for, send_from_directory, render_template_string, Response
import subprocess

app = Flask(__name__)

# Folder with your images
IMAGE_DIR = os.path.abspath(sys.argv[1])
# List of images (filter common extensions)
IMAGES = [f for f in os.listdir(IMAGE_DIR) if f.lower().endswith((".jpg", ".jpeg"))]
IMAGES.sort()
special_keywords_groups = [["delete", "favourite"],
                           ["tadhg", "ciaran", "lisa"],
                           ["baz", "callie", "crackers"],
                           ["silly", "nature", "cuddles"]]
special_keywords = [x for group in special_keywords_groups for x in group]

def special_key_entry(k):
    return """
    <input id="{0}" type="checkbox" name="{0}" value="y" {{{{ "checked" if {0!r} in keywords else "" }}}}>
    <label for="{0}">{0}</label>
    """.format(k)
def other_keywords(keywords):
    return ",".join(set(keywords).difference(special_keywords))
STYLES = """
img, input[type="text"] {
  width: 100%;
}

"""
# --- Template for details.html page ---
DETAILS_TEMPLATE = """
<!DOCTYPE html>
<html>
<head>
    <title>Metadata Editor</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="color-scheme" content="light dark">
    <link rel="stylesheet" href="{{ url_for("styles") }}"> 
</head>
<body>
    <h2>{{ filename }}</h2>
    <img src="{{ url_for('serve_image', filename=filename) }}">

    <form method="GET" action="{{ url_for('post_details', filename=filename) }}">
      <p>
        <label>Caption:</label><br>
        <input type="text" name="caption" value="{{ caption }}">
      </p>
      <p>
        <label>title:</label><br>
        <input type="text" name="title" value="{{ title }}">
      </p>
<p>
%s
</p>
      <p>
        <label>Other Keywords (comma separated):</label><br>
        <input type="text" name="keywords" value="{{ other_keywords(keywords) }}">
      </p>
        <input type="submit" formtarget="_none" value="Save">
        <button type="submit">Save & Next</button>
    </form>
</body>
</html>
""" % "\n</p><p>".join("\n".join(special_key_entry(k) for k in group) for group in special_keywords_groups)

def read_keywords(form):
    for k in special_keywords:
        if form.get(k, False):
            yield k
    others = form.get("keywords","")
    if others:
        yield from others.split(",")

def get_metadata(path, tag, solo=False):
    """Extract IPTC metadata for thee given tag, returns as list"""
    p = subprocess.run(["iptc", "-p", "{}{}".format(tag, "" if solo else ":all"), path], capture_output=True, text=True)
    if p.returncode == 1 and "Could not find dataset" in p.stderr:
        return "" if solo else []
    else:
        p.check_returncode()
        v = p.stdout.strip().splitlines()
        if solo:
            if len(v) > 1:
                raise TypeError("{} has multi valued {} but expected only 1".format(path,tag))
            elif len(v) == 0:
                return ""
            else:
                return v[0]
        return v

def override_metadata(path, **new_tags):
    """Save IPTC tags, for each key override existing tags, if the value is an empty string the tag is just deleted. if the value is a string it is set as the only value. Otherwise the value must be a sequence of strings for multiple values to set"""
    print(path, new_tags)
    for tag,vals in new_tags.items():
        subprocess.run(["iptc", "-d", tag+":all", path])
        if vals == "":
            continue # just delete the tag
        elif isinstance(vals, str):
            subprocess.run(["iptc", "-m", tag, "-v", vals, path], check=True)
        else:
            for v in vals:
                subprocess.run(["iptc", "-a", tag, "-v", v, path], check=True)
@app.route("/aux/styles.css")
def styles():
    return Response(STYLES, mimetype="text/css")
@app.route("/images/<path:filename>")
def serve_image(filename):
    """Serve raw images."""
    return send_from_directory(IMAGE_DIR, filename)

@app.route("/update/<path:filename>")
def post_details(filename):
    path = os.path.join(IMAGE_DIR, filename)
    
    override_metadata(path,
                      Caption=request.args.get("caption",""),
                      Headline=request.args.get("title",""),
                      Keywords=read_keywords(request.args))
    next_index = IMAGES.index(filename) + 1
    if next_index < len(IMAGES):
        return redirect(url_for("details", index=next_index))
    else:
        return "Done! no more images."
    
@app.route("/<int:index>/details.html", methods=["GET"])
def details(index):
    """Show metadata form or update metadata and go to next."""
    if index < 0 or index >= len(IMAGES):
        return "No more images."

    filename = IMAGES[index]
    path = os.path.join(IMAGE_DIR, filename)

    return render_template_string(DETAILS_TEMPLATE,
                                  filename=filename,
                                  caption=get_metadata(path, "Caption", solo=True),
                                  title=get_metadata(path, "Headline", solo=True),
                                  keywords=get_metadata(path,"Keywords"),
                                  other_keywords=other_keywords)

@app.route("/")
def index():
    return redirect(url_for("details", index=0))

if __name__ == "__main__":
    app.run(port=5000, debug=True)
