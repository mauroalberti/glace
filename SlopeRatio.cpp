#include <cstdio>
#include <string>
#include <list>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <vector>
#include <set>
#include <map>


using namespace std;

class result_record
{
    public:
     int id_track;
     double s,x,y,z_orig,z_int;

    private:
};


class Point
{
    public:

        Point();
        Point(int id,  double x,  double y,  double z);
        int id;
        double x, y, z;

        Point&
        operator=(Point p1)
        {
            id = p1.id;
            x = p1.x;
            y = p1.y;
            z = p1.z;

            return *this;

        }


    private:

};

Point::Point()
{
    id = 0;
    x = 0.0;
    y = 0.0;
    z = 0.0;

}

Point::Point(int id_input,  double x_input,  double y_input,  double z_input)
{
    id = id_input;
    x = x_input;
    y = y_input;
    z = z_input;

}


class Point_along_Segment
{

    public:

        Point_along_Segment(double s, Point pt);

        Point pt;
        double s;

    private:





};


Point_along_Segment::Point_along_Segment(double s_input, Point pt_input)
{

    s = s_input;
    pt = pt_input;

}


bool operator<(Point_along_Segment ps1, Point_along_Segment ps2)
{

    return ps1.s < ps2.s;

}


class Vector
{
    public:

        Vector();
        Vector(Point Point0, Point Point1);
        Point VectorStartPoint, VectorEndPoint;

        double magnitude();
        double magnitude_horiz();
        Vector unitvector();

        Vector vectorbyscalar(Vector vector1,  double scalar1);
        Vector vectdiff(Vector vect2);
        Vector vector_projection(Vector vector1, Vector vector2);

        Point vector2point();

        Vector antivector();
        Vector vectorproduct(Vector inputvector2);
        double scalarproduct(Vector inputvector2);
        double scalarproduct_horiz(Vector inputvector2);
        double vectorsangle(Vector inputvector2);
        double vectorsangle_horiz(Vector inputvector2);

    private:


    protected:


};


Vector::Vector()
{

VectorStartPoint = Point();
VectorEndPoint = Point();

}


Vector::Vector(Point Point0, Point Point1)
{

VectorStartPoint = Point0;
VectorEndPoint = Point1;

}



double Vector::magnitude() // magnitude of vector
{

double x_comp =  VectorEndPoint.x - VectorStartPoint.x;
double y_comp =  VectorEndPoint.y - VectorStartPoint.y;
double z_comp =  VectorEndPoint.z - VectorStartPoint.z;

 return sqrt(x_comp*x_comp + y_comp*y_comp + z_comp*z_comp);
}


double Vector::magnitude_horiz() // magnitude of vector
{

double x_comp =  VectorEndPoint.x - VectorStartPoint.x;
double y_comp =  VectorEndPoint.y - VectorStartPoint.y;


 return sqrt(x_comp*x_comp + y_comp*y_comp);

}



Vector Vector::unitvector() // normalization of vector
{

 Point outputvector_secondpoint;

 long double magnit = magnitude();

 outputvector_secondpoint.x = ((VectorEndPoint.x - VectorStartPoint.x)/magnit) + VectorStartPoint.x;
 outputvector_secondpoint.y = ((VectorEndPoint.y - VectorStartPoint.y)/magnit) + VectorStartPoint.y;
 outputvector_secondpoint.z = ((VectorEndPoint.z - VectorStartPoint.z)/magnit) + VectorStartPoint.z;


 return Vector(VectorStartPoint, outputvector_secondpoint);
}

Vector Vector::antivector() // vector opposite
{
    Vector antivect;

    antivect.VectorStartPoint = VectorEndPoint;
    antivect.VectorEndPoint = VectorStartPoint;

    return antivect;

}


Point Vector::vector2point() // vector opposite
{
    Point convert_vect;

    convert_vect.x = VectorEndPoint.x - VectorStartPoint.x;
    convert_vect.y = VectorEndPoint.y - VectorStartPoint.y;
    convert_vect.z = VectorEndPoint.z - VectorStartPoint.z;


    return convert_vect;

}



double Vector::scalarproduct(Vector inputvector2) // function scalarproduct
{

Point conv_vect1 = vector2point();
Point conv_vect2 = inputvector2.vector2point();


 //calculations
 return (conv_vect1.x*conv_vect2.x) + (conv_vect1.y*conv_vect2.y) + (conv_vect1.z*conv_vect2.z);


}

double Vector::scalarproduct_horiz(Vector inputvector2) // function scalarproduct
{

Point conv_vect1 = vector2point();
Point conv_vect2 = inputvector2.vector2point();


 //calculations
 return (conv_vect1.x*conv_vect2.x) + (conv_vect1.y*conv_vect2.y);


}


double Vector::vectorsangle(Vector inputvector2) // function vectorsangle output in radians
{

    double magnitude_v1 = magnitude();
    double magnitude_v2 = inputvector2.magnitude();

    double minimum_magnitude = 1.0e-5;

    if (magnitude_v1 < minimum_magnitude || magnitude_v2 < minimum_magnitude) return -1.0;  // undetermined result, due to too small vector(s)

    double cos_vectorsangle = scalarproduct(inputvector2)/(magnitude_v1*magnitude_v2);

    double vectorsangle_rad;

     if (cos_vectorsangle > 1.0) vectorsangle_rad = 0.0;
     else if  (cos_vectorsangle < -1.0) vectorsangle_rad = M_PI;
     else vectorsangle_rad = acos(cos_vectorsangle);

     return vectorsangle_rad;

}


double Vector::vectorsangle_horiz(Vector inputvector2) // function vectorsangle output in radians
{

    double magnitude_horiz_v1 = magnitude_horiz();
    double magnitude_horiz_v2 = inputvector2.magnitude_horiz();

    double minimum_magnitude = 1.0e-5;

    if (magnitude_horiz_v1 < minimum_magnitude || magnitude_horiz_v2 < minimum_magnitude) return -1.0;  // undetermined result, due to too small vector(s)

    double cos_vectorsangle_horiz = scalarproduct_horiz(inputvector2)/(magnitude_horiz_v1*magnitude_horiz_v2);

    double vectorsangle_rad_horiz;

     if (cos_vectorsangle_horiz > 1.0) vectorsangle_rad_horiz = 0.0;
     else if  (cos_vectorsangle_horiz < -1.0) vectorsangle_rad_horiz = M_PI;
     else vectorsangle_rad_horiz = acos(cos_vectorsangle_horiz);

     return vectorsangle_rad_horiz;

}


void interpolate_megadune_data(list<Point_along_Segment> PointsAlongMegaduneTrack, double interpol_elevations[], int mylistlength)
{

    interpol_elevations[mylistlength];

    double separation_threshold = 100; // meters

    list<Point_along_Segment>:: iterator ptsalsegm_pos;

    int num_recs_withoutelevation = 0;

    int n=0;

    double dist_elev_array[mylistlength][2];

    for (ptsalsegm_pos = PointsAlongMegaduneTrack.begin(); ptsalsegm_pos != PointsAlongMegaduneTrack.end(); ptsalsegm_pos++)
    {


        dist_elev_array[n][0] = (*ptsalsegm_pos).s;
        dist_elev_array[n][1] = (*ptsalsegm_pos).pt.z;

        if (abs(dist_elev_array[n][1]) < 0.001) num_recs_withoutelevation++;

        cout << "mydata = " << dist_elev_array[n][0] << "   " << dist_elev_array[n][1] << "\n";


        n++;

    }


    int num_recs_withelevation = mylistlength - num_recs_withoutelevation;

    cout << "\nnum_recs_withelevation = " << num_recs_withelevation << "\n";
    cout << "num_recs_withoutelevation = " << num_recs_withoutelevation << "\n";


    double max_elevation = 0.0;

    for (int i=0; i<mylistlength; i++)
    {
        if (dist_elev_array[i][1] > max_elevation) max_elevation=dist_elev_array[i][1];
    }
    cout << "max_elevation = " << max_elevation << "\n\n";


    /// PROCESSAMENTI DELLE ELEVAZIONI


    // A - determinazione del massimo in un intorno predefinito dal punto considerato


    for (int i=0; i<mylistlength; i++)
    {

        double local_maximum = dist_elev_array[i][1];



        for (int j = i-1; j >= 0; j--)
        {

            double separation = abs(dist_elev_array[i][0] - dist_elev_array[j][0]);
            if (separation > separation_threshold) break;

            if (dist_elev_array[j][1] > local_maximum) local_maximum = dist_elev_array[j][1];

        }

        for (int j = i+1; j <mylistlength; j++)
        {
            double separation = abs(dist_elev_array[i][0] - dist_elev_array[j][0]);
            if (separation > separation_threshold) break;

            if (dist_elev_array[j][1] > local_maximum) local_maximum = dist_elev_array[j][1];

        }

        interpol_elevations[i] = local_maximum;


    }


    // B - interpolazione lineare dei valori di elevazione


    for (int i=0; i<mylistlength; i++)
    {

        cout << "i = " << i << "\n";


        if (interpol_elevations[i] > 1) continue;


        // inizializzazione dei valori down-e up-track

        double downtrack_elevations[2][2], uptrack_elevations[2][2]; // first index: separation; second index: elevation

        for (int k = 0; k <= 1; k++)
        {
            for (int l = 0; l <= 1; l++)
            {
                 downtrack_elevations[k][l] = -1.0;
                 uptrack_elevations[k][l] = -1.0;
            }
        }

        int ndx_dwtr_validelevs = -1, ndx_uptr_validelevs = -1;


        // calcolo dei valori downtrack

        for (int j = i-1; j >= 0; j--)
        {

            if (interpol_elevations[j] > 1)
            {

                ndx_dwtr_validelevs++;
                downtrack_elevations[ndx_dwtr_validelevs][0] = abs(dist_elev_array[i][0]-dist_elev_array[j][0]);
                downtrack_elevations[ndx_dwtr_validelevs][1] = interpol_elevations[j];


            }

            if (ndx_dwtr_validelevs >= 1) break;


        }

        // calcolo dei valori uptrack

        for (int j = i+1; j < mylistlength; j++)
        {

            if (interpol_elevations[j] > 1)
            {

                ndx_uptr_validelevs++;
                uptrack_elevations[ndx_uptr_validelevs][0] = abs(dist_elev_array[i][0]-dist_elev_array[j][0]);
                uptrack_elevations[ndx_uptr_validelevs][1] = interpol_elevations[j];


            }

            if (ndx_uptr_validelevs >= 1) break;

        }


        // gestione dei casi risultanti

        if (ndx_dwtr_validelevs == -1 && ndx_uptr_validelevs == -1) continue;  // nessuna elevazione valida ai due lati
        if (ndx_dwtr_validelevs >= 0 && ndx_uptr_validelevs >= 0)   // almeno una elevazione valida ad ognuno dei lati della posizione corrente
        {
            double myleftseparation = downtrack_elevations[0][0];
            double myleftelevation = downtrack_elevations[0][1];

            double myrightseparation = uptrack_elevations[0][0];
            double myrightelevation = uptrack_elevations[0][1];

            double myelevationdiff = myrightelevation - myleftelevation;
            double myvalidelevsseparation = myleftseparation + myrightseparation;

            double myinterpolateddeltaelev = (myelevationdiff/myvalidelevsseparation)*myleftseparation;

            interpol_elevations[i] = myleftelevation + myinterpolateddeltaelev;

            continue;

        }
        if (ndx_dwtr_validelevs >= 0) interpol_elevations[i] = downtrack_elevations[0][1];
        if (ndx_uptr_validelevs >= 0) interpol_elevations[i] = uptrack_elevations[0][1];


    }

    return;

}



list<result_record> process_megadune_data(Point mymegadunetrace[], int mymegadunetrace_numrecs, Point myelevpts[], int myelevpts_num)
{

    double pi = 3.141593;

    list<Point_along_Segment> PointsAlongMegaduneTrack;

    Point myMegaduneTrace_FirstPoint = mymegadunetrace[0];
    //printf("Point 1:  %d, %f, %f, %f\n", myMegaduneTrace_FirstPoint.id, myMegaduneTrace_FirstPoint.x, myMegaduneTrace_FirstPoint.y, myMegaduneTrace_FirstPoint.z);

    Point_along_Segment ptalongsegment0(0, myMegaduneTrace_FirstPoint);
    // storing con sorting del vertice iniziale v1
    PointsAlongMegaduneTrack.push_back(ptalongsegment0);

    double cumulated_length = 0.0;

    for (int i = 1; i < mymegadunetrace_numrecs; i++)
    {

        // definizione e iniziali inserimenti nel set che conserva i risultati puntuali delle elevazioni lungo il segmento considerato

        set<Point_along_Segment> PtsAlongSegment; // set di punti lungo la traccia, con distanza dall'inizio del segmento

        Point myMegaduneTrace_SecondPoint = mymegadunetrace[i];
        // creazione vettore v1
        Vector v1(myMegaduneTrace_FirstPoint, myMegaduneTrace_SecondPoint);
        // calcolo magnitudine v1
        double magnitude_horiz_v1 = v1.magnitude_horiz();


        // storing con sorting del vertice finale p2
        Point_along_Segment ptalongsegment1(cumulated_length+magnitude_horiz_v1, myMegaduneTrace_SecondPoint);
        PtsAlongSegment.insert(ptalongsegment1);



        for (int j = 0; j < myelevpts_num; j++)
        {


            Point myElevationPoint = myelevpts[j];

            //printf("myMegaduneTrace_FirstPoint:  %d, %f, %f, %f\n", myMegaduneTrace_FirstPoint.id, myMegaduneTrace_FirstPoint.x, myMegaduneTrace_FirstPoint.y, myMegaduneTrace_FirstPoint.z);
            //printf("myElevationPoint:  %d, %f, %f, %f\n", myElevationPoint.id, myElevationPoint.x, myElevationPoint.y, myElevationPoint.z);


            // creazione vettore v2
            Vector v2(myMegaduneTrace_FirstPoint, myElevationPoint);

            // calcolo magnitudine v2
            double magnitude_horiz_v2 = v2.magnitude_horiz();

            //printf("magnitude v2: %f\n\n", magnitude_horiz_v2);

            // calcolo coseno angolo sotteso da v1 e v2  = (v1 x v2 )/(|v1| |v2|)
            double angle_sotteso_rad = v1.vectorsangle_horiz(v2);
            double angle_sotteso_degr = angle_sotteso_rad*180.0/pi;

            if (angle_sotteso_degr > 90.0) continue;

            //printf("angle_sotteso_degr: %f\n\n", angle_sotteso_degr);

            // calcolo proiezione di v2 su v1 = v2 cos(alpha)

            double lungh_proiezione_v2_su_v1 = magnitude_horiz_v2*cos(angle_sotteso_rad);

            // se lunghezza proiezione <= lunghezza v1, elevazione incorporata
            if (lungh_proiezione_v2_su_v1 > magnitude_horiz_v1) continue;

            //printf("lungh_proiezione_v2_su_v1: %f\n\n", lungh_proiezione_v2_su_v1);

            // determinazione x_pr, y_pr da proporzionalità

            double coeff_propor = lungh_proiezione_v2_su_v1/magnitude_horiz_v1;

            double delta_x = (myMegaduneTrace_SecondPoint.x - myMegaduneTrace_FirstPoint.x)*coeff_propor;
            double delta_y = (myMegaduneTrace_SecondPoint.y - myMegaduneTrace_FirstPoint.y)*coeff_propor;


            // creazione di punto di output

            Point Projected_Elevation_Point(myMegaduneTrace_FirstPoint.id, myMegaduneTrace_FirstPoint.x+delta_x, myMegaduneTrace_FirstPoint.y+delta_y, myElevationPoint.z);
            //printf("Projected_Elevation_Point:  %d, %f, %f, %f\n\n", Projected_Elevation_Point.id, Projected_Elevation_Point.x, Projected_Elevation_Point.y, Projected_Elevation_Point.z);


            // creazione e storing con sorting del punto di elevazione proiettato, assieme alla relativa distanza, in base alla distanza da v1
            Point_along_Segment ptalongsegment(cumulated_length+lungh_proiezione_v2_su_v1, Projected_Elevation_Point);
            PtsAlongSegment.insert(ptalongsegment);

            //Vector ProjectionVector(myElevationPoint, Projected_Elevation_Point);

        }


        // storing di p2 in p1, per successivo ciclo
        myMegaduneTrace_FirstPoint = myMegaduneTrace_SecondPoint;
        // updating cumulated lenght for subsequent cycle
        cumulated_length = cumulated_length + magnitude_horiz_v1;


        //  processamento e trasferimento dei dati in un vettore/lista di PtsAlongSegment
        set<Point_along_Segment>:: iterator ptsalsegm_pos;
        for (ptsalsegm_pos = PtsAlongSegment.begin(); ptsalsegm_pos != PtsAlongSegment.end(); ptsalsegm_pos++)
        {
            PointsAlongMegaduneTrack.push_back(*ptsalsegm_pos);
        }



    }

    // gestione dei casi di segmento senza alcun punto proiettato
    // determinazione di elevazione di P0 e P1, tenuto conto dell'ultimo valore del caso precedente e del primo della coppia successiva
    // output dei punti interpolati


    int mylistlength = PointsAlongMegaduneTrack.size();
    double interpol_elevations[mylistlength];

    interpolate_megadune_data(PointsAlongMegaduneTrack, interpol_elevations, mylistlength);

    list<result_record> megadunetrace_result;

    list<Point_along_Segment>:: iterator pos; int n = 0;
    for (pos = PointsAlongMegaduneTrack.begin(); pos != PointsAlongMegaduneTrack.end(); pos++)
    {
        result_record myelevationresult;

        myelevationresult.id_track = (*pos).pt.id;
        myelevationresult.s = (*pos).s;
        myelevationresult.x = (*pos).pt.x;
        myelevationresult.y = (*pos).pt.y;
        myelevationresult.z_orig = (*pos).pt.z;
        myelevationresult.z_int = interpol_elevations[n];

        megadunetrace_result.push_back(myelevationresult);

        n++;

    }



    return megadunetrace_result;


}


int main ()
{

    int i,j;


    /* definizione del file di input dei tracciati */
    cout << "Enter feature line input file name: ";
  	string filename_tracciati;
    cin >> filename_tracciati;

    ifstream infile_tracciati;
    infile_tracciati.open(filename_tracciati.c_str(), ios::binary);

    if (infile_tracciati.fail())
    {
    cout << "File di input " << filename_tracciati << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }



    /* definizione del file di input dei punti di elevazione */
    cout << "Enter elevation point input file name: ";
  	string filename_pt_elevazione;
    cin >> filename_pt_elevazione;

    ifstream infile_pt_elevazione;
    infile_pt_elevazione.open(filename_pt_elevazione.c_str(), ios::binary);

    if (infile_pt_elevazione.fail())
    {
    cout << "File di input " << filename_pt_elevazione << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }


    // definizione del file di output puntuale
    cout << "Enter output line file name: ";
  	string output_linefilename;
    cin >> output_linefilename;

    ofstream outpointfile;
    outpointfile.open(output_linefilename.c_str(), ios::binary);

    if (outpointfile.fail())
    {
    cout << "File di output " << output_linefilename << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }


    //
    // inizio processamento input dati
    //


    // LETTURA DATI DA FILE INPUT TRACCE

    set<int> set_mdid;
    multiset<int> multiset_mdid;
    string rec_line;

    // legge l'header del file
    getline(infile_tracciati, rec_line);

    list<Point> megadune_tracepts;

    int md_id; double md_x, md_y;

    char comma;


    int num_recs_tracc = 0;

    // lettura dei dati di input delle tracce di megadune
    while (!infile_tracciati.eof())
    {

        // read record
        getline(infile_tracciati, rec_line);

        if (rec_line.size() == 0) continue;

        num_recs_tracc++;

        istringstream instr(rec_line);

        instr >> md_id >> comma >> md_x >> comma >> md_y;
        //printf("controllo:  %d, %f, %f\n", md_id, md_x, md_y);
        set_mdid.insert(md_id);
        multiset_mdid.insert(md_id); // inserisce id della megaduna in lista con id univoci

        Point pt(md_id, md_x, md_y, 0.0);

        //printf("controllo traccie megadune:  %d, %f, %f\n", pt.id, pt.x, pt.y);

        megadune_tracepts.push_back(pt);


    }


    // chiude file di input tracciati megadune
    infile_tracciati.close();


    list<Point>::iterator pos; // iteratore della lista

   // LETTURA DATI DA FILE INPUT ELEVATION POINTS


    // legge l'header del file
    getline(infile_pt_elevazione, rec_line);

    int num_recs_elevpts = 0;

    list<Point> megadune_elevpts;

    int ep_id; long double ep_x, ep_y, ep_z;

    // lettura dei dati di input dei punti di elevazione
    while (!infile_pt_elevazione.eof())
    {

        // read record
        getline(infile_pt_elevazione, rec_line);

        if (rec_line.size() == 0) continue;

        num_recs_elevpts++;
        istringstream instr(rec_line);

        instr >> ep_id >> comma >> ep_x >> comma >> ep_y >> comma >> ep_z;

        Point pt; pt.id = ep_id; pt.x = ep_x; pt.y = ep_y; pt.z = ep_z;
        megadune_elevpts.push_back(pt);

    }

    // chiude file di input elevazioni punti
    infile_pt_elevazione.close();


    // output su schermo di dati riepilogativi
    cout << "num recs tracce: " << num_recs_tracc << "; num recs elevpts: " << num_recs_elevpts << "\n";


    // ---------------------------------

    // pre-processamento dati


    int num_trace_recs_exam, num_elev_pts_exam;

    map <int, int> map_mdtrace_recs, map_elevpts_recs;

    set<int>::iterator p, q;

    for (p = set_mdid.begin(); p != set_mdid.end(); p++)
    {


        int exam_megadune_id = *p;
        int exam_megadune_idnumrecs = 0;

        for (q = multiset_mdid.begin(); q != multiset_mdid.end(); q++)
        {
            if (*q == exam_megadune_id) exam_megadune_idnumrecs++;
        }

        map_mdtrace_recs[exam_megadune_id] = exam_megadune_idnumrecs;



        int exam_elevpts_numrecs = 0;

        for (pos=megadune_elevpts.begin(); pos!=megadune_elevpts.end(); pos++)
        {

            if ((*pos).id == exam_megadune_id) exam_elevpts_numrecs++;


        }


        map_elevpts_recs[exam_megadune_id] = exam_elevpts_numrecs;


    }

    cout << "numero vertici tracce megadune\n";

    map<int,int>::iterator m;

    for (m = map_mdtrace_recs.begin(); m != map_mdtrace_recs.end(); m++)
    {
          printf("  %d, %d\n", (*m).first, (*m).second);
    }


    cout << "numero elevation points\n";

     for (m = map_elevpts_recs.begin(); m != map_elevpts_recs.end(); m++)
    {
          printf("  %d, %d\n", (*m).first, (*m).second);
    }



    // processamento dati


    outpointfile << "id_rec, id_track, s, x, y, z_orig, z_int\n";


    int id_record_result = 0;




    for (m = map_mdtrace_recs.begin(); m != map_mdtrace_recs.end(); m++)
    {


        // identificativo e numero vertici di megaduna analizzata
        int anal_megadune_id = (*m).first;   // analysed megadune id
        int anal_megadune_numrecs = (*m).second;



        // definizione vertici singola traccia di megaduna analizzata
        Point mymegadunetrace[anal_megadune_numrecs];    // array of Points that will store the megadune trace vertices

        int id_mdtr_subset = 0;

        for (pos=megadune_tracepts.begin(); pos!=megadune_tracepts.end(); pos++)
        {

            if ((*pos).id == anal_megadune_id)
            {
                mymegadunetrace[id_mdtr_subset].id = (*pos).id;
                mymegadunetrace[id_mdtr_subset].x = (*pos).x;
                mymegadunetrace[id_mdtr_subset].y = (*pos).y;
                id_mdtr_subset++;
            }

        }


        // definizione dati elevazioni lungo una singola traccia di megaduna
        int myelevpts_megaduna_num = map_elevpts_recs[anal_megadune_id];

        Point myelevpts[myelevpts_megaduna_num];

        int id_elevpts_subset = 0;

        for (pos=megadune_elevpts.begin(); pos!=megadune_elevpts.end(); pos++)
        {

            if ((*pos).id == anal_megadune_id)
            {
                myelevpts[id_elevpts_subset].id = (*pos).id;
                myelevpts[id_elevpts_subset].x = (*pos).x;
                myelevpts[id_elevpts_subset].y = (*pos).y;
                myelevpts[id_elevpts_subset].z = (*pos).z;
                id_elevpts_subset++;
            }

        }

        // analisi elevation points along a megadune trace
        list<result_record> megadunetrace_result = process_megadune_data(mymegadunetrace, anal_megadune_numrecs, myelevpts, myelevpts_megaduna_num);


        // output dei risultati
        list<result_record>::iterator pos_res;

        for (pos_res=megadunetrace_result.begin(); pos_res!=megadunetrace_result.end(); pos_res++)
        {

            id_record_result++;

            outpointfile << id_record_result << "," << (*pos_res).id_track << "," <<
                        (*pos_res).s << "," << (*pos_res).x << "," << (*pos_res).y << "," <<
                        (*pos_res).z_orig << "," << (*pos_res).z_int << "\n";


        }




    }

    outpointfile.close();

    return 0;

}




