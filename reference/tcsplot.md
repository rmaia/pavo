# Interactive plot of a tetrahedral colourspace

Produces an interactive 3D plot of a tetrahedral colourspace using
OpenGL capabilities.

Plots points in a tetrahedral colour space

## Usage

``` r
tcsplot(
  tcsdata,
  size = 0.02,
  alpha = 1,
  col = "black",
  vertexsize = 0.02,
  achro = TRUE,
  achrosize = 0.01,
  achrocol = "grey",
  lwd = 1,
  lcol = "lightgrey",
  new = FALSE,
  hspin = FALSE,
  vspin = FALSE,
  floor = TRUE,
  gamut = FALSE
)

tcspoints(tcsdata, size = 0.02, col = "black", alpha = 1)

tcsvol(
  tcsdata,
  type = c("convex", "alpha"),
  avalue = "auto",
  col = "black",
  alpha = 0.2,
  grid.alpha = 1,
  grid = TRUE,
  fill = TRUE,
  lwd = 1
)
```

## Arguments

- tcsdata:

  (required) a data frame, possibly a result from the
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) or
  [`tcspace()`](https://pavo.colrverse.com/reference/tcspace.md)
  function, containing values for the 'x', 'y' and 'z' coordinates as
  columns (labeled as such).

- size:

  size of the points in the plot (defaults to 0.02)

- alpha:

  transparency of points (or volume fill in `tcsvol()`)

- col:

  colour of the points in the plot (defaults to black)

- vertexsize:

  size of the points at the vertices

- achro:

  should a point be plotted at the origin (defaults to `TRUE`)?

- achrosize:

  size of the point at the origin when `achro = TRUE` (defaults to
  `0.8`).

- achrocol:

  color of the point at the origin `achro = TRUE` (defaults to
  `'grey'`).

- lwd, lcol:

  graphical parameters for the edges of the tetrahedron.

- new:

  should a new 3D plot be called (defaults to `FALSE`)?

- hspin:

  if `TRUE`, the graphic will spin horizontally (around the 'z'
  axis)(defaults to `FALSE`)

- vspin:

  if `TRUE`, the graphic will spin vertically (around the 'x'
  axis)(defaults to `FALSE`)

- floor:

  if `TRUE`, a reference xy plane is plotted under the tetrahedron
  (defaults to `TRUE`)

- gamut:

  logical. Should the polygon showing the possible colours given visual
  system and illuminant used in the analysis (defaults to `FALSE`). This
  option currently only works when `qcatch = Qi`.

- type:

  accepts a vector of length 1 or 2 with 'p' for points and/or 'l' for
  lines from the point to the base of the tetrahedron.

- avalue:

  if `type = "alpha"`, which alpha parameter value should be used to
  compute the alphashape. `avalue = "auto"` (default) finds and use the
  \\\alpha^\*\\ value as defined in Gruson (2020).

- grid.alpha:

  transparency of the volume polygon grid lines

- grid:

  if `TRUE`, connects the polygon outlining the volume occupied by
  points (defaults to `TRUE`)

- fill:

  if `TRUE`, fills the volume occupied by points (WARNING: transparency
  is not saved properly if exported using `rgl.postscript`)(defaults to
  `TRUE`).

## Value

`tcsplot()` creates a 3D plot using functions of the package rgl, based
on openGL capabilities. Plot is interactive and can be manipulated with
the mouse (left button: rotate along 'z' axis; right button: rotate
along 'x' axis; third button: zoom).

`tcspoints()` adds points to the plot. Points are currently plotted only
as spheres to maintain export capabilities.

`tcsvol()` creates a 3D colour volume within a `tcsplot` object.

## References

Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
in a tetrahedral color space: A phylogenetic analysis of new world
buntings. The American Naturalist, 171(6), 755-776.

Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as
birds see them. Biological Journal Of The Linnean Society, 86(4),
405-431.

## Author

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
# For plotting
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
tcsplot(tcs.sicalis, size = 0.005)
rgl::rgl.postscript("testplot.pdf", fmt = "pdf")
#> Warning: Postscript conversion failed
rgl::rgl.snapshot("testplot.png")

# For adding points
patch <- rep(c("C", "T", "B"), 7)
tcs.crown <- subset(tcs.sicalis, "C")
tcs.breast <- subset(tcs.sicalis, "B")
tcsplot(tcs.crown, col = "blue")
tcspoints(tcs.breast, col = "red")
3D plot

{"x":{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":true,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmode":"modulate","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0],"margin":"","floating":false,"tag":"","blend":["src_alpha","one_minus_src_alpha"]},"rootSubscene":1,"objects":{"54":{"id":54,"type":"spheres","material":{"lit":false},"vertices":"0","colors":"1","radii":[[0.01999999955296516]],"centers":"2","ignoreExtent":false,"fastTransparency":true,"flags":32770},"56":{"id":56,"type":"text","material":{"lit":false,"margin":0,"floating":true,"edge":[0,1,1]},"vertices":"3","colors":"4","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"5","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"57":{"id":57,"type":"text","material":{"lit":false,"margin":1,"floating":true,"edge":[1,1,1]},"vertices":"6","colors":"7","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"8","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"58":{"id":58,"type":"text","material":{"lit":false,"margin":2,"floating":true,"edge":[1,1,1]},"vertices":"9","colors":"10","texts":[[""]],"cex":[[1]],"adj":[[0.5,0.5,0.5]],"centers":"11","family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":33808},"59":{"id":59,"type":"linestrip","material":{"lit":false},"vertices":"12","colors":"13","centers":"14","ignoreExtent":false,"flags":32832},"60":{"id":60,"type":"spheres","material":{"lit":false},"vertices":"15","colors":"16","radii":[[0.009999999776482582]],"centers":"17","ignoreExtent":false,"fastTransparency":true,"flags":32770},"61":{"id":61,"type":"spheres","material":{"lit":false},"vertices":"18","colors":"19","radii":[[0.01999999955296516]],"centers":"20","ignoreExtent":false,"fastTransparency":true,"flags":32770},"62":{"id":62,"type":"quads","material":{"lit":false,"front":"lines","back":"lines"},"vertices":"21","colors":"23","centers":"24","normals":"22","ignoreExtent":false,"flags":32770},"63":{"id":63,"type":"spheres","material":{"lit":false},"vertices":"25","colors":"26","radii":[[0.01999999955296516]],"centers":"27","ignoreExtent":false,"fastTransparency":true,"flags":32770},"5":{"id":5,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"7":{"id":7,"type":"background","material":{"lit":false,"back":"lines"},"colors":"28","centers":"29","sphere":false,"fogtype":"none","fogscale":1,"flags":32768},"55":{"id":55,"type":"bboxdeco","material":{"front":"culled","back":"culled"},"colors":"30","axes":{"mode":["none","none","none"],"step":[-1,-1,-1],"nticks":[0,0,0],"marklen":[15,15,15],"expand":[1.029999971389771,1.029999971389771,1.029999971389771]},"draw_front":false,"flags":32769},"1":{"id":1,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":1,"mouseMode":{"none":"none","left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,5.67280101776123],"modelMatrix":[[0.8975274562835693,0,0,0],[0,0.3544613122940063,1.032949686050415,-0.3304259479045868],[0,-0.9738744497299194,0.3759629428386688,-5.51734447479248],[0,0,0,1]],"projMatrix":[[3.732050895690918,0,0,0],[0,3.732050895690918,0,0],[0,0,-3.863703012466431,-20.44979095458984],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201433256682,0.9396926207859085,0],[0,-0.9396926207859085,0.3420201433256682,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[0.8975274562835693,1.036375522613525,1.099242091178894],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[-0.699999988079071,0.699999988079071,-0.5,1,-0.300000011920929,0.7681943774223328],"windowRect":[0,0,256,256],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"NULL","maxClipPlanes":2147483647,"glVersion":"NA","activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[7,55,54,56,57,58,59,60,61,62,63,5],"subscenes":[],"flags":34131}},"crosstalk":{"key":[],"group":[],"id":[],"options":[]},"width":700,"height":432.6328800988875,"buffer":{"accessors":[{"bufferView":0,"componentType":5126,"count":4,"type":"VEC3"},{"bufferView":1,"componentType":5121,"count":4,"type":"VEC4","normalized":true},{"bufferView":2,"componentType":5126,"count":4,"type":"VEC3"},{"bufferView":3,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":4,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":5,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":6,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":7,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":8,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":9,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":10,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":11,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":12,"componentType":5126,"count":8,"type":"VEC3"},{"bufferView":13,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":14,"componentType":5126,"count":8,"type":"VEC3"},{"bufferView":15,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":16,"componentType":5126,"count":1,"type":"VEC4"},{"bufferView":17,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":18,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":19,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":20,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":21,"componentType":5126,"count":4,"type":"VEC3"},{"bufferView":22,"componentType":5121,"count":4,"type":"VEC3"},{"bufferView":23,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":24,"componentType":5126,"count":1,"type":"VEC3"},{"bufferView":25,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":26,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":27,"componentType":5126,"count":7,"type":"VEC3"},{"bufferView":28,"componentType":5121,"count":1,"type":"VEC4"},{"bufferView":29,"componentType":5121,"count":1,"type":"VEC3"},{"bufferView":30,"componentType":5121,"count":1,"type":"VEC4"}],"bufferViews":[{"buffer":0,"byteLength":48,"byteOffset":0},{"buffer":0,"byteLength":16,"byteOffset":48},{"buffer":0,"byteLength":48,"byteOffset":64},{"buffer":0,"byteLength":12,"byteOffset":112},{"buffer":0,"byteLength":4,"byteOffset":124},{"buffer":0,"byteLength":12,"byteOffset":128},{"buffer":0,"byteLength":12,"byteOffset":140},{"buffer":0,"byteLength":4,"byteOffset":152},{"buffer":0,"byteLength":12,"byteOffset":156},{"buffer":0,"byteLength":12,"byteOffset":168},{"buffer":0,"byteLength":4,"byteOffset":180},{"buffer":0,"byteLength":12,"byteOffset":184},{"buffer":0,"byteLength":96,"byteOffset":196},{"buffer":0,"byteLength":16,"byteOffset":292},{"buffer":0,"byteLength":96,"byteOffset":308},{"buffer":0,"byteLength":3,"byteOffset":404},{"buffer":0,"byteLength":16,"byteOffset":408},{"buffer":0,"byteLength":3,"byteOffset":424},{"buffer":0,"byteLength":84,"byteOffset":428},{"buffer":0,"byteLength":4,"byteOffset":512},{"buffer":0,"byteLength":84,"byteOffset":516},{"buffer":0,"byteLength":48,"byteOffset":600},{"buffer":0,"byteLength":12,"byteOffset":648},{"buffer":0,"byteLength":4,"byteOffset":660},{"buffer":0,"byteLength":12,"byteOffset":664},{"buffer":0,"byteLength":84,"byteOffset":676},{"buffer":0,"byteLength":4,"byteOffset":760},{"buffer":0,"byteLength":84,"byteOffset":764},{"buffer":0,"byteLength":4,"byteOffset":848},{"buffer":0,"byteLength":3,"byteOffset":852},{"buffer":0,"byteLength":4,"byteOffset":855}],"buffers":[{"byteLength":859,"bytes":"AAAAAAAAAAAAAEA/ccQcv/MEtb4AAIC+AAAAAPMENT8AAIC+ccQcP/MEtb4AAIC+mE6j/zd+\nuP9Nr0r/5Boc/wAAAAAAAAAAAABAP3HEHL/zBLW+AACAvgAAAADzBDU/AACAvnHEHD/zBLW+\nAACAvgAAwH8AAIBAAACAPwAAAAEAAMB/AACAQAAAgD8AAMB/AACAQAAAgD8AAAABAADAfwAA\ngEAAAIA/AADAfwAAgEAAAIA/AAAAAQAAwH8AAIBAAACAPwAAAAAAAAAAAABAP3HEHL/zBLW+\nAACAvgAAAADzBDU/AACAvnHEHD/zBLW+AACAvnHEHL/zBLW+AACAvnHEHD/zBLW+AACAvgAA\nAAAAAAAAAABAPwAAAADzBDU/AACAvtTTUz/U01M/1NNTPwAAgD8AAAAAAAAAAAAAQD9xxBy/\n8wS1vgAAgL4AAAAA8wQ1PwAAgL5xxBw/8wS1vgAAgL5xxBy/8wS1vgAAgL5xxBw/8wS1vgAA\ngL4AAAAAAAAAAAAAQD8AAAAA8wQ1PwAAgL4AAAAAv74+P7++Pj+/vj4/AACAPwAAAABO1Y0+\nuSnBPbY4G74vX40+lwPNPctdGL5YtZU+91bTPePiSr4Nr5Y+PwDMPRhGR74BzZQ+xtDgPecD\nQL6fQIM+WrzJPRoqQb5irZY+sd/QPQoUR74AAAEBTtWNPrkpwT22OBu+L1+NPpcDzT3LXRi+\nWLWVPvdW0z3j4kq+Da+WPj8AzD0YRke+Ac2UPsbQ4D3nA0C+n0CDPlq8yT0aKkG+Yq2WPrHf\n0D0KFEe+MzMzvwAAAL+amZm+MzMzPwAAAL+amZm+MzMzPwAAgD+amZm+MzMzvwAAgD+amZm+\nAAABAAABAAABAAABAAAAAQAAAAAAAIA+mpmZvqR2hD4ebuI9JAj/vUfvdj5pDds9V5Tevc7d\nYD6fPNI9hCTIvYlziz4hUec9CIUYvhCgYT4QwMo9DxW6vclbJD7j8Z09CXh8vaMsdj5Kq+o9\ne2EGvgEAAAGkdoQ+Hm7iPSQI/71H73Y+aQ3bPVeU3r3O3WA+nzzSPYQkyL2Jc4s+IVHnPQiF\nGL4QoGE+EMDKPQ8Vur3JWyQ+4/GdPQl4fL2jLHY+SqvqPXthBr4BAQEBAAAAAAAAAQ=="}]},"context":{"shiny":false,"rmarkdown":null},"vertexShader":"#line 2 1\n// File 1 is the vertex shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\n\nattribute vec3 aPos;\nattribute vec4 aCol;\nuniform mat4 mvMatrix;\nuniform mat4 prMatrix;\nvarying vec4 vCol;\nvarying vec4 vPosition;\n\n#ifdef NEEDS_VNORMAL\nattribute vec3 aNorm;\nuniform mat4 normMatrix;\nvarying vec4 vNormal;\n#endif\n\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nattribute vec2 aTexcoord;\nvarying vec2 vTexcoord;\n#endif\n\n#ifdef FIXED_SIZE\nuniform vec3 textScale;\n#endif\n\n#ifdef FIXED_QUADS\nattribute vec3 aOfs;\n#endif\n\n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\nvarying float normz;\nuniform mat4 invPrMatrix;\n#else\nattribute vec3 aPos1;\nattribute vec3 aPos2;\nvarying float normz;\n#endif\n#endif // IS_TWOSIDED\n\n#ifdef FAT_LINES\nattribute vec3 aNext;\nattribute vec2 aPoint;\nvarying vec2 vPoint;\nvarying float vLength;\nuniform float uAspect;\nuniform float uLwd;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  \n#ifndef IS_BRUSH\n#if defined(NCLIPPLANES) || !defined(FIXED_QUADS) || defined(HAS_FOG) || defined(USE_ENVMAP)\n  vPosition = mvMatrix * vec4(aPos, 1.);\n#endif\n  \n#ifndef FIXED_QUADS\n  gl_Position = prMatrix * vPosition;\n#endif\n#endif // !IS_BRUSH\n  \n#ifdef IS_POINTS\n  gl_PointSize = POINTSIZE;\n#endif\n  \n  vCol = aCol;\n  \n// USE_ENVMAP implies NEEDS_VNORMAL\n\n#ifdef NEEDS_VNORMAL\n  vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));\n#endif\n\n#ifdef USE_ENVMAP\n  vReflection = normalize(reflect(vPosition.xyz/vPosition.w, \n                        normalize(vNormal.xyz/vNormal.w)));\n#endif\n  \n#ifdef IS_TWOSIDED\n#ifdef HAS_NORMALS\n  /* normz should be calculated *after* projection */\n  normz = (invPrMatrix*vNormal).z;\n#else\n  vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n  pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n  vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n  pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n  normz = pos1.x*pos2.y - pos1.y*pos2.x;\n#endif\n#endif // IS_TWOSIDED\n  \n#ifdef NEEDS_VNORMAL\n  vNormal = vec4(normalize(vNormal.xyz), 1);\n#endif\n  \n#if defined(HAS_TEXTURE) || defined(IS_TEXT)\n  vTexcoord = aTexcoord;\n#endif\n  \n#if defined(FIXED_SIZE) && !defined(ROTATING)\n  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w;\n  gl_Position = pos + vec4(aOfs*textScale, 0.);\n#endif\n  \n#if defined(IS_SPRITES) && !defined(FIXED_SIZE)\n  vec4 pos = mvMatrix * vec4(aPos, 1.);\n  pos = pos/pos.w + vec4(aOfs,  0.);\n  gl_Position = prMatrix*pos;\n#endif\n  \n#ifdef FAT_LINES\n  /* This code was inspired by Matt Deslauriers' code in \n   https://mattdesl.svbtle.com/drawing-lines-is-hard */\n  vec2 aspectVec = vec2(uAspect, 1.0);\n  mat4 projViewModel = prMatrix * mvMatrix;\n  vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n  currentProjected = currentProjected/currentProjected.w;\n  vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n  vec2 currentScreen = currentProjected.xy * aspectVec;\n  vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n  float len = uLwd;\n  vec2 dir = vec2(1.0, 0.0);\n  vPoint = aPoint;\n  vLength = length(nextScreen - currentScreen)/2.0;\n  vLength = vLength/(vLength + len);\n  if (vLength > 0.0) {\n    dir = normalize(nextScreen - currentScreen);\n  }\n  vec2 normal = vec2(-dir.y, dir.x);\n  dir.x /= uAspect;\n  normal.x /= uAspect;\n  vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n  gl_Position = currentProjected + offset;\n#endif\n  \n#ifdef IS_BRUSH\n  gl_Position = vec4(aPos, 1.);\n#endif\n}","fragmentShader":"#line 2 2\n// File 2 is the fragment shader\n#ifdef GL_ES\n#ifdef GL_FRAGMENT_PRECISION_HIGH\nprecision highp float;\n#else\nprecision mediump float;\n#endif\n#endif\nvarying vec4 vCol; // carries alpha\nvarying vec4 vPosition;\n#if defined(HAS_TEXTURE) || defined (IS_TEXT)\nvarying vec2 vTexcoord;\nuniform sampler2D uSampler;\n#endif\n\n#ifdef HAS_FOG\nuniform int uFogMode;\nuniform vec3 uFogColor;\nuniform vec4 uFogParms;\n#endif\n\n#if defined(IS_LIT) && !defined(FIXED_QUADS)\nvarying vec4 vNormal;\n#endif\n\n#if NCLIPPLANES > 0\nuniform vec4 vClipplane[NCLIPPLANES];\n#endif\n\n#if NLIGHTS > 0\nuniform mat4 mvMatrix;\n#endif\n\n#ifdef IS_LIT\nuniform vec3 emission;\nuniform float shininess;\n#if NLIGHTS > 0\nuniform vec3 ambient[NLIGHTS];\nuniform vec3 specular[NLIGHTS]; // light*material\nuniform vec3 diffuse[NLIGHTS];\nuniform vec3 lightDir[NLIGHTS];\nuniform bool viewpoint[NLIGHTS];\nuniform bool finite[NLIGHTS];\n#endif\n#endif // IS_LIT\n\n#ifdef IS_TWOSIDED\nuniform bool front;\nvarying float normz;\n#endif\n\n#ifdef FAT_LINES\nvarying vec2 vPoint;\nvarying float vLength;\n#endif\n\n#ifdef USE_ENVMAP\nvarying vec3 vReflection;\n#endif\n\nvoid main(void) {\n  vec4 fragColor;\n#ifdef FAT_LINES\n  vec2 point = vPoint;\n  bool neg = point.y < 0.0;\n  point.y = neg ? (point.y + vLength)/(1.0 - vLength) :\n                 -(point.y - vLength)/(1.0 - vLength);\n#if defined(IS_TRANSPARENT) && defined(IS_LINESTRIP)\n  if (neg && length(point) <= 1.0) discard;\n#endif\n  point.y = min(point.y, 0.0);\n  if (length(point) > 1.0) discard;\n#endif // FAT_LINES\n  \n#ifdef ROUND_POINTS\n  vec2 coord = gl_PointCoord - vec2(0.5);\n  if (length(coord) > 0.5) discard;\n#endif\n  \n#if NCLIPPLANES > 0\n  for (int i = 0; i < NCLIPPLANES; i++)\n    if (dot(vPosition, vClipplane[i]) < 0.0) discard;\n#endif\n    \n#ifdef FIXED_QUADS\n    vec3 n = vec3(0., 0., 1.);\n#elif defined(IS_LIT)\n    vec3 n = normalize(vNormal.xyz);\n#endif\n    \n#ifdef IS_TWOSIDED\n    if ((normz <= 0.) != front) discard;\n#endif\n\n#ifdef IS_LIT\n    vec3 eye = normalize(-vPosition.xyz/vPosition.w);\n    vec3 lightdir;\n    vec4 colDiff;\n    vec3 halfVec;\n    vec4 lighteffect = vec4(emission, 0.);\n    vec3 col;\n    float nDotL;\n#ifdef FIXED_QUADS\n    n = -faceforward(n, n, eye);\n#endif\n    \n#if NLIGHTS > 0\n    // Simulate two-sided lighting\n    if (n.z < 0.0)\n      n = -n;\n    for (int i=0;i<NLIGHTS;i++) {\n      colDiff = vec4(vCol.rgb * diffuse[i], vCol.a);\n      lightdir = lightDir[i];\n      if (!viewpoint[i]) {\n        if (finite[i]) {\n          lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n        } else {\n          lightdir = (mvMatrix * vec4(lightdir, 0.)).xyz;\n        }\n      }\n      if (!finite[i]) {\n        halfVec = normalize(lightdir + eye);\n      } else {\n        lightdir = normalize(lightdir - vPosition.xyz/vPosition.w);\n        halfVec = normalize(lightdir + eye);\n      }\n      col = ambient[i];\n      nDotL = dot(n, lightdir);\n      col = col + max(nDotL, 0.) * colDiff.rgb;\n      col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular[i];\n      lighteffect = lighteffect + vec4(col, colDiff.a);\n    }\n#else\n    lighteffect.a = 1.;\n#endif\n    \n#else // not IS_LIT\n    vec4 colDiff = vCol;\n    vec4 lighteffect = colDiff;\n#endif\n    \n#ifdef IS_TEXT\n    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n#endif\n    \n#ifdef HAS_TEXTURE\n\n// These calculations use the definitions from \n// https://docs.gl/gl3/glTexEnv\n\n#ifdef USE_ENVMAP\n    float m = 2.0 * sqrt(dot(vReflection, vReflection) + 2.0*vReflection.z + 1.0);\n    vec4 textureColor = texture2D(uSampler, vReflection.xy / m + vec2(0.5, 0.5));\n#else\n    vec4 textureColor = texture2D(uSampler, vTexcoord);\n#endif\n\n#ifdef TEXTURE_rgb\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(textureColor.rgb, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*vec4(textureColor.rgb, 1.);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb, lighteffect.a);\n#endif\n\n#endif //TEXTURE_rgb\n        \n#ifdef TEXTURE_rgba\n\n#ifdef TEXMODE_replace\n// already done\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = lighteffect*textureColor;\n#endif\n\n#ifdef TEXMODE_decal\n    textureColor = vec4((1. - textureColor.a)*lighteffect.rgb) +\n                     textureColor.a*textureColor.rgb, \n                     lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(lighteffect.rgb + textureColor.rgb,\n                    lighteffect.a*textureColor.a);\n#endif\n    \n#endif //TEXTURE_rgba\n    \n#ifdef TEXTURE_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(lighteffect.rgb, luminance);\n#endif \n\n#if defined(TEXMODE_modulate) || defined(TEXMODE_blend) || defined(TEXMODE_add)\n    textureColor = vec4(lighteffect.rgb, lighteffect.a*luminance);\n#endif\n \n#endif // TEXTURE_alpha\n    \n// The TEXTURE_luminance values are not from that reference    \n#ifdef TEXTURE_luminance\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, lighteffect.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, lighteffect.a);\n#endif\n\n#endif // TEXTURE_luminance\n \n    \n#ifdef TEXTURE_luminance_alpha\n    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n\n#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n    textureColor = vec4(luminance, luminance, luminance, textureColor.a);\n#endif \n\n#ifdef TEXMODE_modulate\n    textureColor = vec4(luminance*lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_blend\n    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n                        textureColor.a*lighteffect.a);\n#endif\n\n#ifdef TEXMODE_add\n    textureColor = vec4(luminance + lighteffect.rgb, \n                        textureColor.a*lighteffect.a);\n\n#endif\n\n#endif // TEXTURE_luminance_alpha\n    \n    fragColor = textureColor;\n\n#elif defined(IS_TEXT)\n    if (textureColor.a < 0.1)\n      discard;\n    else\n      fragColor = textureColor;\n#else\n    fragColor = lighteffect;\n#endif // HAS_TEXTURE\n    \n#ifdef HAS_FOG\n    // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))\n    // In Exp and Exp2: use density = density/far\n    // fogF will be the proportion of fog\n    // Initialize it to the linear value\n    float fogF;\n    if (uFogMode > 0) {\n      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n      if (uFogMode > 1)\n        fogF = mix(uFogParms.w, 1.0, fogF);\n      fogF = fogF*uFogParms.z;\n      if (uFogMode == 2)\n        fogF = 1.0 - exp(-fogF);\n      // Docs are wrong: use (density*c)^2, not density*c^2\n      // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58\n      else if (uFogMode == 3)\n        fogF = 1.0 - exp(-fogF*fogF);\n      fogF = clamp(fogF, 0.0, 1.0);\n      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n    } else gl_FragColor = fragColor;\n#else\n    gl_FragColor = fragColor;\n#endif // HAS_FOG\n    \n}","players":[],"webGLoptions":{"preserveDrawingBuffer":true},"fastTransparency":true},"evals":[],"jsHooks":[]}
# For plotting convex hull
tcsplot(tcs.sicalis, col = "blue", size = 0.005)
tcsvol(tcs.sicalis)
```
