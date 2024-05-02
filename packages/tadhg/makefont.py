import fontforge

UNICODE_INDEX_FOR_ZERO = 0x3358

BOX_SIZE = 200
MARGIN_HEIGHT = 0
BORDER_SIZE = 40

# height of font
EM = MARGIN_HEIGHT+BOX_SIZE*4 + BORDER_SIZE*3

class MyFont:
    def __init__(self, name, familyname, fullname=None,comment=None):
        f = fontforge.font()
        f.fontname = name
        f.familyname = familyname
        f.fullname = fullname or name
        f.comment = comment
        f.em = EM
        f.ascent = EM
        self.underlying_font = f
    def new_glyph(self, ordinal, name):
        glyph = self.underlying_font.createChar(ordinal, name)
        return MyGlyph(glyph)
    def save(self, filename):
        self.underlying_font.generate(filename)
    def __enter__(self):return self
    def __exit__(self, *tb_info):
        self.save(self.fontname+".ttf")

class MyGlyph:
    # the width between characters, all positions are offset by half of this right before drawing
    MARGIN_WIDTH = 100
    MARGIN_HEIGHT = 0
    def __init__(self, underlying_glyph):
        self.underlying_glyph = underlying_glyph
        self.pen = underlying_glyph.glyphPen()
        self.max_x = 0;
    def gen_point(self, x, y):
        return (x+self.MARGIN_WIDTH/2, y+self.MARGIN_HEIGHT)
    def fill_rect(self, x,y,w,h):
        """fills in a rectangle with a corner at (x,y) with width 'w' and height 'h'.
        Note the coordinates will be shifted by MARGIN_WIDTH/2 and MARGIN_HEIGHT respectively
        note that w and h must be positive to work properly."""
        self.pen.moveTo(self.gen_point(x,y))
        self.pen.lineTo(self.gen_point(x+w,y))
        self.pen.lineTo(self.gen_point(x+w,y+h))
        self.pen.lineTo(self.gen_point(x,y+h))
        self.pen.closePath()
        self.max_x = max([self.max_x, x, x+w])
    def sub_rect(self,x,y,w,h):
        """same syntax as fill_rect but subtracts the space of this rectangle."""
        # whether it is added or subtracted is based on whether the points are defined clockwise or counterclockwise
        # so starting at x+w and moving -w in x direction reverses the direction moved.
        self.fill_rect(x+w,y,-w,h)
    def finish(self):
        # calculate the width based on the maximum x value used + the full margin width.
        self.underlying_glyph.width = self.max_x + self.MARGIN_WIDTH
        # docs have it remove the reference to pen to allow ui to update, probably not necessary but might as well.
        self.pen = None
    def __exit__(self,*tb_info):self.finish()
    def __enter__(self):return self
    
    

def boxes_filled(number):
    """
    returns string with subset of characters "abcd"
    where with the substitution a=1, b=2, c=3, d=6 the sum of elements gives the original number
    This corresponds to the boxes to fill in in the dozonal display.
    """
    return set(_boxes_filled_helper(number))
def _boxes_filled_helper(number):
    if number == 12:
        yield from 'abcd'
        return
    if number > 12 or number < 0:
        raise ValueError(f"number must be in range [0,11), got {number!r}")
    if number >= 6:
        yield "d"
        number -= 6
    if number >=3:
        yield "c"
        number -= 3
    yield from ['','a','b'][number]

def draw_char_simple(font, ord):
    with font.new_glyph(UNICODE_INDEX_FOR_ZERO+ord, "doz{}".format(ord)) as g:
        g.fill_rect(0,0,BOX_SIZE*3+BORDER_SIZE*3, EM)
        boxes = boxes_filled(ord)
        if 'a' not in boxes:
            g.sub_rect(BORDER_SIZE, 2*BORDER_SIZE+3*BOX_SIZE, BOX_SIZE, BOX_SIZE)
        if 'b' not in boxes:
            g.sub_rect(2*BORDER_SIZE+BOX_SIZE, 2*BORDER_SIZE+3*BOX_SIZE, 2*BOX_SIZE, BOX_SIZE)
        if 'c' not in boxes:
            g.sub_rect(BORDER_SIZE, BORDER_SIZE, BOX_SIZE, 3*BOX_SIZE)
        if 'd' not in boxes:
            g.sub_rect(2*BORDER_SIZE+BOX_SIZE, BORDER_SIZE, 2*BOX_SIZE, 3*BOX_SIZE)

font = MyFont("dozonal", "numerals", comment="dozonal characters made by Tadhg McDonald-Jensen")

for num in range(13):
    draw_char_simple(font, num)
font.save("test.ttf")
