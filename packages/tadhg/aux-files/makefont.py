"""
This defines a dozonal font based on a box with aspect ratio w/h = 3/4 split into 4 regions:
- A is a 1x1 box
- B is a 2x1 box
- C is a 1x3 box
- D is a 2x3 box (area of 6)

The boxes are filled in to represent 'n' in interval [0,12) according to:
- A = (n%3==1)
- B = (n%3==2)
- C = (n-6*D) >= 3   I.E. (3<=n<6 or 9<=n<12)
- D = (n >= 6)

These are arranged into a glyph like so:

+-----+---+
|  B  | A |
+-----+---+
|     |   |
|     |   |
|  D  | C |
|     |   |
|     |   |
+-----+---+

Originally it was mirrored so A was left of B and C left of D but when using this font to write a clock having D closer to the more significant digit to the left is more useful.

"""



import fontforge

UNICODE_INDEX_FOR_ZERO = 0x3358
# number of pixels width and height of the A box, thus the whole glyph will be 3*BOX_SIZE wide and 4*BOX_SIZE tall if border width was 0.
BOX_SIZE = 200
MARGIN_HEIGHT = 0
# The width of the outer "wall" of the glyph and the space between boxes within the glyph, meaning this contributes to the total width and height of the glyph 3 times.
BORDER_SIZE = 40

# height of font
EM = MARGIN_HEIGHT+BOX_SIZE*4 + BORDER_SIZE*3

class MyFont:
    "wrapper around fontforge font to use MyGlyph wrapper"
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
    MARGIN_HEIGHT = MARGIN_HEIGHT
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
    def contour_from_complex(self, iter_of_complex_positions):
        first = next(iter_of_complex_positions)
        xs = {self.max_x, first.real}
        self.pen.moveTo(self.gen_point(first.real, first.imag))
        for p in iter_of_complex_positions:
            self.pen.lineTo(self.gen_point(p.real, p.imag))
            xs.add(p.real)
        self.max_x = max(xs)
        self.pen.closePath()
    def finish(self):
        # calculate the width based on the maximum x value used + the full margin width.
        self.underlying_glyph.width = self.max_x + self.MARGIN_WIDTH
        # docs have it remove the reference to pen to allow ui to update, probably not necessary but might as well.
        self.pen = None
    def __exit__(self,*tb_info):self.finish()
    def __enter__(self):return self
    
    

def boxes_filled(number):
    """
    returns set containing some collection of the characters "abcd"
    where with the substitution a=1, b=2, c=3, d=6 the sum of elements gives the original number
    This corresponds to the boxes to fill in in the dozonal display.
    """
    return set(_boxes_filled_helper(number))
def _boxes_filled_helper(number):
    if number == 12:
        # not strictly proper for number system but useful to have defined at the font level, can be used as overflow or NAN etc
        yield from 'abcd'
        return
    if number > 12 or number < 0:
        raise ValueError(f"number must be in range [0,12], got {number!r}")
    if number >= 6:
        yield "d"
        number -= 6
    if number >=3:
        yield "c"
        number -= 3
    yield from ['','a','b'][number]


## Old version that has visible seperators between boxes
## didn't like the style, found that leaving the space between filled boxes empty made it easier to see
# def draw_char_simple(g, num):
#     g.fill_rect(0,0,BOX_SIZE*3+BORDER_SIZE*3, EM)
#     boxes = boxes_filled(num)
#     if 'a' not in boxes:
#         g.sub_rect(BORDER_SIZE, 2*BORDER_SIZE+3*BOX_SIZE, BOX_SIZE, BOX_SIZE)
#     if 'b' not in boxes:
#         g.sub_rect(2*BORDER_SIZE+BOX_SIZE, 2*BORDER_SIZE+3*BOX_SIZE, 2*BOX_SIZE, BOX_SIZE)
#     if 'c' not in boxes:
#         g.sub_rect(BORDER_SIZE, BORDER_SIZE, BOX_SIZE, 3*BOX_SIZE)
#     if 'd' not in boxes:
#         g.sub_rect(2*BORDER_SIZE+BOX_SIZE, BORDER_SIZE, 2*BOX_SIZE, 3*BOX_SIZE)

def Dr(box_multiple, border_multiple=0):
    "helper to get A*BOX_SIZE + B*BORDER_SIZE for a given (A,B)"
    return box_multiple*BOX_SIZE + border_multiple*BORDER_SIZE
def Dj(box_multiple, border_multiple=0):
    "same as Dr but returns as a imaginary unit"
    return 1j * Dr(box_multiple, border_multiple)
def cutout_points_helper(num):
    """yields complex numbers where real part is X coordinate and imaginary part is Y coordinate
    they are all relative to the inner border of a glyph, so Dr(0,1)+Dj(0,1) should be added to all the points to draw it in the proper place (and the margin)
    """
    from collections import namedtuple
    # the premise of this function is to fill in the outer border of the glyph and then cut out one contour to hollow out the empty numbers
    # leave the filled in boxes but still cut out a gap for the inner border
    # so we trace a contour in counter clockwise direction, for each box we move horizontally and vertically and which one we do first
    # will decide whether the box is cut out or left full, so the BoxDef defines
    # both distances we move, which one we do first based on whether box is full or empty and then the shift over the border distance after to move to the starting position of the next box.
    BoxDef = namedtuple("BoxDef", ["label", "empty_dist", "filled_dist", "border_shift"])
    # points is the list of points at the corners of each box clockwise BACD
    points = [
        # B, start at (3,0) go up if empty and right if filled
        BoxDef("b", Dj(1), Dr(2), Dr(0,1)),
        # A, start top left corner, go right if empty and down if filled
        BoxDef("a", Dr(1), Dj(-1), Dj(0,-1)),
        # C, start top right, go down if empty, go left if filled
        BoxDef("c", Dj(-3), Dr(-1), Dr(0,-1)),
        # D, start bottom right, go left if empty and up if filled
        BoxDef("d", Dr(-2), Dj(3), Dj(0,1))
    ]
    boxes_to_fill = boxes_filled(num)
    # start at bottom left corner of box B
    pos = start = Dr(0,1) + Dj(3,2)
    for label, empty_dist, filled_dist, border_dist in points:
        print(pos, label, empty_dist, filled_dist, border_dist)
        yield pos
        yield pos + (filled_dist if label in boxes_to_fill else empty_dist)
        pos += filled_dist + empty_dist
        yield pos
        pos += border_dist
    print(pos, "\n\n")
    assert start.real == pos.real and start.imag == pos.imag, f"did not return to starting position: {start} {pos}"


def make_cutout_glyph(font, num):
    g.fill_rect(0,0,Dr(3,3), Dr(4,3))
    g.contour_from_complex(cutout_points_helper(num))
        


    
    

font = MyFont("dozonal", "numerals", comment="dozonal characters made by Tadhg McDonald-Jensen")

for num in range(13):
    with font.new_glyph(UNICODE_INDEX_FOR_ZERO+num, "doz{}".format(num)) as g:
        make_cutout_glyph(g, num)
font.save("test.ttf")
