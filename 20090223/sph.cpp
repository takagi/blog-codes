
#include <fstream>
#include <algorithm>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <boost/format.hpp>

#define FOR_EACH_PARTICLES( ps, p ) \
  for( Particles::iterator p = ps->begin(); p != ps->end(); p++ )

typedef boost::numeric::ublas::vector<double> vec;
typedef struct
{
  vec    r;
  vec    v;
  double rho;
  double p;
  vec    f;
} Particle;
typedef std::vector<Particle> Particles;

int MAX_LOOP;
double PI;
double H;
double SPH_PMASS;
double SPH_INTSTIFF;
double SPH_EXTSTIFF;
double SPH_EXTDAMP;
double Poly6Kern;
double SpikyKern;
double LapKern;
double DT;
double RADIUS;
double EPSILON;
vec    INITMIN(3);
vec    INITMAX(3);
vec    MIN(3);
vec    MAX(3);
double SPH_PDIST;
double SPH_SIMSCALE;
double SPH_RESTDENSITY;
double SPH_VISC;
double SPH_LIMIT;

double norm2( vec& a )
{
    return a(0)*a(0) + a(1)*a(1) + a(2)*a(2);
}

double norm( vec& a )
{
    return sqrt( norm2( a ) );
}


void init( Particles* ps )
{
  MAX_LOOP  = 1000;
  PI        = 3.1415926535;
  H         = 0.01; // m
  SPH_PMASS = 0.00020543; // kg
  SPH_INTSTIFF = 1.00;
  SPH_EXTSTIFF = 10000.0;
  SPH_EXTDAMP  = 256.0;
  Poly6Kern = 315.0 / ( 64.0 * PI * pow( H, 9 ) );
  SpikyKern = -45.0 / ( PI * pow( H, 6 ) );
  LapKern   = 45.0 / ( PI * pow( H, 6 ) );
  DT        = 0.004;
  RADIUS    = 0.004; // m
  EPSILON   = 0.00001;
  INITMIN(0) =  0.0; INITMIN(1) =  0.0; INITMIN(2) = 0.0;
  INITMAX(0) = 10.0; INITMAX(1) = 20.0; INITMAX(2) = 0.0;
  MIN(0) =  0.0; MIN(1) =  0.0; MIN(2) = -10.0;
  MAX(0) = 20.0; MAX(1) = 20.0; MAX(2) =  10.0;
  SPH_RESTDENSITY = 600.0; // kg / m^3
  SPH_PDIST = pow( SPH_PMASS / SPH_RESTDENSITY, 1/3.0 );
  SPH_SIMSCALE = 0.004; // unit size
  SPH_VISC = 0.2; // pascal-second (Pa.s) = 1 kg m^-1 s^-1
  SPH_LIMIT = 200.0;
  
  double d;
  d = SPH_PDIST * 0.87 / SPH_SIMSCALE;
  
  Particle p;
  for ( double z = INITMIN(2); z <= INITMAX(2); z += d ) {
    for ( double y = INITMIN(1); y <= INITMAX(1); y += d ) {
      for ( double x = INITMIN(0); x <= INITMAX(0); x += d ) {
        p.r = vec(3); p.v = vec(3); p.f = vec(3);
        p.r(0) = x; p.r(1) = y; p.r(2) = z;
        p.r(0) += -0.05 + rand() * 0.1 / RAND_MAX;
        p.r(1) += -0.05 + rand() * 0.1 / RAND_MAX;
        p.r(2) += -0.05 + rand() * 0.1 / RAND_MAX;
        p.v(0) = 0.0; p.v(1) = 0.0; p.v(2) = 0.0;
        ps->push_back( p );
      }
    }
  }
  std::cout << ps->size() << std::endl;
}

void calculate_density_and_pressure( Particles* ps )
{
  double r2, c, sum, H2;
  vec dr;
  
  H2 = H*H;
    
  FOR_EACH_PARTICLES( ps, p )
  {
    sum = 0.0;
    FOR_EACH_PARTICLES( ps, pj )
    {
      if ( p == pj ) continue;
      dr = (p->r - pj->r) * SPH_SIMSCALE;
      r2 = norm2( dr );
      if ( H2 > r2 )
      {
        c = H2 - r2;
        sum += c * c * c;
      }
    }
    p->rho = sum * SPH_PMASS * Poly6Kern;
    std::cout << "rho " << p->rho << std::endl;
    p->p   = ( p->rho - SPH_RESTDENSITY ) * SPH_INTSTIFF;
    std::cout << "p   " << p->p << std::endl;
    p->rho = 1.0 / p->rho;  // take inverse for later calculation
  }
}

void calculate_force( Particles* ps )
{
  double pterm, vterm, r, c;
  vec dr(3), force(3), fcurr(3);
  
  FOR_EACH_PARTICLES( ps, p )
  {
    force(0) = 0.0; force(1) = 0.0; force(2) = 0.0;
    FOR_EACH_PARTICLES( ps, pj )
    {
      if ( p == pj ) continue;
      dr = ( p->r - pj->r ) * SPH_SIMSCALE;
      r = norm( dr );
      //std::cout << p->r << std::endl;
      //std::cout << pj->r << std::endl;
      //std::cout << dr << std::endl;
      //std::cout << r << std::endl;
      if ( H > r )
      {
        c = H - r;
        pterm = -0.5 * c * SpikyKern * ( p->p + pj->p ) / r;
        vterm = LapKern * SPH_VISC;
        fcurr = pterm * dr + vterm * ( pj->v - p->v );
        fcurr *= c * p->rho * pj->rho;
        force += fcurr;
      }
    }
    std::cout << force << std::endl;
    p->f = force;
  }
}

void calculate_position( Particles* ps )
{
  vec accel(3), g(3), norm(3);
  double speed, diff, adj;
  
  g(0) = 0.0; g(1) = -9.8; g(2) = 0.0;
  FOR_EACH_PARTICLES( ps, p )
  {
    std::cout << p->f * SPH_PMASS << "\n";
    accel = p->f * SPH_PMASS;  // why "*"?

    speed = norm2( accel );
    if ( speed > SPH_LIMIT*SPH_LIMIT ) {
        accel *= SPH_LIMIT / sqrt(speed);
    }

    // Z-axis walls
    diff = 2.0 * RADIUS - ( p->r(2) - MIN(2) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = 0.0; norm(1) = 0.0; norm(2) = 1.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    diff = 2.0 * RADIUS - ( MAX(2) - p->r(2) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = 0.0; norm(1) = 0.0; norm(2) = -1.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    
    // X-axis walls
    diff = 2.0 * RADIUS - ( p->r(0) - MIN(0) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = 1.0; norm(1) = 0.0; norm(2) = 0.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    diff = 2.0 * RADIUS - ( MAX(0) - p->r(0) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = -1.0; norm(1) = 0.0; norm(2) = 0.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    
    // Y-axis walls
    diff = 2.0 * RADIUS - ( p->r(1) - MIN(1) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = 0.0; norm(1) = 1.0; norm(2) = 0.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    diff = 2.0 * RADIUS - ( MAX(1) - p->r(1) ) * SPH_SIMSCALE;
    if ( diff > EPSILON )
    {
        norm(0) = 0.0; norm(1) = -1.0; norm(2) = 0.0;
        adj = SPH_EXTSTIFF * diff - SPH_EXTDAMP * inner_prod( norm, p->v );
        accel += adj * norm;
    }
    
    accel += g;
    p->v += accel * DT;
    p->r += p->v * DT / SPH_SIMSCALE;
    
    // stick on x-y plane
    p->r(2) = 0.0;
    
  }
}

void process( Particles* ps )
{
  calculate_density_and_pressure( ps );
  calculate_force( ps );
  calculate_position( ps );
}

void output_particles( Particles* ps )
{
  static int num = 0;
  std::string file_name = (boost::format( "result%08d.pov" ) % num++ ).str();
  std::cout << "processing " << file_name << " ..." << std::endl;
  std::ofstream f( file_name.c_str() );
  if ( ! f )
  {
    std::cerr << "cannot open " << file_name << std::endl;
    exit(1);
  }
  
  f << "#include \"colors.inc\"\n"
    << "camera {\n"
    << "  location <10, 5, -40.0>\n"
    << "  look_at <10, 5, 0.0>\n"
    << "}\n"
    << "light_source { <0, 30, -30> color White }\n";
  FOR_EACH_PARTICLES( ps, p )
  {
    f << "sphere {\n"
      << "  <" << p->r(0) << ", " << p->r(1) << ", " << p->r(2) << ">, 0.5\n"
      << "  texture {\n"
      << "    pigment { color Yellow }\n"
      << "  }\n"
      << "}\n";
  }
  f << std::endl;
}

void output( Particles* ps )
{
  output_particles( ps );
}

void _main( Particles* ps, int loop )
{
  if ( loop > 0 )
  {
    process( ps );
    output( ps );
    _main( ps, loop - 1 );
  }
  else
    return;
}

int main()
{
  Particles ps;
  init( &ps );
  output( &ps );
  _main( &ps, MAX_LOOP );
  return 0;
}
