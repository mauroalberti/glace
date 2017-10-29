#include <cstdio>
#include <string>
#include <list>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <vector>
#include <set>
#include <map>
#include <cassert>


using namespace std;

class result_record
{
    public:
     int id_track;
     double s,h,x,y,z_orig,z_proxint,z_linint,dist_proxelpt;

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

        Point_along_Segment(double s, Point pt, double h);

        Point pt;
        double s, h;  // s: distance along segment, h: distance from segment

    private:





};


Point_along_Segment::Point_along_Segment(double s_input, Point pt_input, double h_input)
{

    s = s_input;
    pt = pt_input;
    h = h_input;

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


void interpolate_megaduna_elevations(list<Point_along_Segment> PointsAlongMegaduna, double elev_linearinterp[], double elev_proximinterp[], double proxinter_distelpt[], int numtotpts_in_megaduna, float myNullDataZ, double filtering_distance)
{

    list<Point_along_Segment>::iterator ptsalsegm_pos;

    int ndx_pt_along_megaduna = 0;

    double megaduna_inputdata[numtotpts_in_megaduna][3];

    int megaduna_id;

    for (ptsalsegm_pos = PointsAlongMegaduna.begin(); ptsalsegm_pos != PointsAlongMegaduna.end(); ptsalsegm_pos++)
    {

        megaduna_id = (*ptsalsegm_pos).pt.id;

        megaduna_inputdata[ndx_pt_along_megaduna][0] = (*ptsalsegm_pos).s;
        megaduna_inputdata[ndx_pt_along_megaduna][1] = (*ptsalsegm_pos).pt.z;
        megaduna_inputdata[ndx_pt_along_megaduna][2] = (*ptsalsegm_pos).h;

        ndx_pt_along_megaduna++;

    }

    assert(ndx_pt_along_megaduna == numtotpts_in_megaduna);

    double megaduna_length = megaduna_inputdata[ndx_pt_along_megaduna][numtotpts_in_megaduna-1];


    /// PROCESSAMENTI DELLE ELEVAZIONI

    // dichiara ed inizializza elev_filtered a valore nullo
    double elev_filtered[numtotpts_in_megaduna];
    for (int i = 0; i < numtotpts_in_megaduna; i++)
    {
        elev_filtered[i] = myNullDataZ;
    }

    // attribuisce ad ogni punto il suo indice rispetto alla distanza di filtering
    int ndx_filtering[numtotpts_in_megaduna];
    for (int i = 0; i < numtotpts_in_megaduna; i++)
    {
        ndx_filtering[i] = int(floor(megaduna_inputdata[i][0]/filtering_distance));
    }

    // crea set che conserva l'elenco ordinato degli indici del filter
    set<int> ndx_filter;
    for (int i = 0; i < numtotpts_in_megaduna; i++)
    {
        ndx_filter.insert(ndx_filtering[i]);
    }

    // crea ed inizializza map che conserva gli indici con le relative minime distanze
    map<int, double> min_dist;
    set<int>::iterator setint_it;
    for (setint_it=ndx_filter.begin(); setint_it != ndx_filter.end(); setint_it++)
    {
        min_dist[*setint_it] = 99999.9;

    }

    // determina la minima distanza nell'intervallo considerato
    for (int i = 0; i < numtotpts_in_megaduna; i++)
    {
        if (abs(megaduna_inputdata[i][1] - myNullDataZ) > 1) // caso di punto con valore valido
        {
            if (megaduna_inputdata[i][2] < min_dist[ndx_filtering[i]]) min_dist[ndx_filtering[i]] = megaduna_inputdata[i][2];
        }
    }



    // setta l'elevazione filtrata all'elevazione osservata quando punto più vicino nell'intervallo
    for (int i = 0; i < numtotpts_in_megaduna; i++)
    {
        if (abs(megaduna_inputdata[i][1] - myNullDataZ) > 1) // caso di punto con valore valido
        {
            if (abs(megaduna_inputdata[i][2] - min_dist[ndx_filtering[i]]) < 0.1) elev_filtered[i] = megaduna_inputdata[i][1];
        }
    }


    /// pre-processamento elevazioni valide

    // determinazione degli indici di elevazioni note, precedenti  e successive, lungo la megaduna

    int fore_validelevation_ndx = -1;
    int fore_validelevations_indices[numtotpts_in_megaduna];

    for (int i=0; i<numtotpts_in_megaduna; i++)
    {

        if (abs(elev_filtered[i] - myNullDataZ) > 1) // caso di punto con valore valido
        {
            fore_validelevation_ndx = i;
        }

        fore_validelevations_indices[i] = fore_validelevation_ndx;

     }

    int back_validelevation_ndx = numtotpts_in_megaduna;
    int back_validelevations_indices[numtotpts_in_megaduna];

    for (int i=numtotpts_in_megaduna-1; i>=0; i--)
    {

        if (abs(elev_filtered[i] - myNullDataZ) > 1) // caso di punto con valore valido
        {
            back_validelevation_ndx = i;
        }

        back_validelevations_indices[i] = back_validelevation_ndx;

     }


    /// B - interpolazioni di prossimità e lineare dei valori di elevazione nulli


    elev_linearinterp[numtotpts_in_megaduna];
    elev_proximinterp[numtotpts_in_megaduna];
    proxinter_distelpt[numtotpts_in_megaduna];

    for (int i=0; i<numtotpts_in_megaduna; i++)
    {

        if (abs(elev_filtered[i] - myNullDataZ) > 1) // caso di punto con valore valido

        {

            elev_proximinterp[i] = elev_filtered[i];
            elev_linearinterp[i] = elev_filtered[i];
            proxinter_distelpt[i] = 0.0;

        }

        else

        {


            double curr_pt_s = megaduna_inputdata[i][0];

            /// interpolazioni

            if ((fore_validelevations_indices[i] == -1) && (back_validelevations_indices[i] == numtotpts_in_megaduna) ) // caso con nessun punto di elevazione disponibile
            {

                elev_linearinterp[i] = myNullDataZ;
                elev_proximinterp[i] = myNullDataZ;
                proxinter_distelpt[i] = 99999.9;

            }
            else if (fore_validelevations_indices[i] == -1)
            {


                elev_proximinterp[i] = elev_filtered[back_validelevations_indices[i]];
                elev_linearinterp[i] = elev_proximinterp[i];

                double back_valelev_s = megaduna_inputdata[back_validelevations_indices[i]][0];
                double back_separation = back_valelev_s - curr_pt_s;
                assert(back_separation >= 0.0);

                proxinter_distelpt[i] = back_separation;

            }
            else if (back_validelevations_indices[i] == numtotpts_in_megaduna)
            {

                elev_proximinterp[i] = elev_filtered[fore_validelevations_indices[i]];
                elev_linearinterp[i] = elev_proximinterp[i];

                double fore_valelev_s = megaduna_inputdata[fore_validelevations_indices[i]][0];
                double fore_separation = curr_pt_s - fore_valelev_s;
                assert(fore_separation >= 0.0);

                proxinter_distelpt[i] = fore_separation;

            }
            else
            {

                double fore_valelev_s = megaduna_inputdata[fore_validelevations_indices[i]][0];
                double back_valelev_s = megaduna_inputdata[back_validelevations_indices[i]][0];
                double curr_pt_s = megaduna_inputdata[i][0];

                double fore_separation = curr_pt_s - fore_valelev_s;
                assert(fore_separation >= 0.0);
                double back_separation = back_valelev_s - curr_pt_s;
                assert(back_separation >= 0.0);

                double fore_elevation = elev_filtered[fore_validelevations_indices[i]];
                double back_elevation = elev_filtered[back_validelevations_indices[i]];

                if (fore_separation < back_separation)
                {
                    elev_proximinterp[i] = fore_elevation;
                    proxinter_distelpt[i] = fore_separation;

                }
                else
                {
                    elev_proximinterp[i] = back_elevation;
                    proxinter_distelpt[i] = back_separation;
                }


                /// interpolazione lineare

                double myelevationdiff = back_elevation - fore_elevation;
                double myvalidelevsseparation = fore_separation + back_separation;

                double myinterpolateddeltaelev = (myelevationdiff/myvalidelevsseparation)*fore_separation;

                elev_linearinterp[i] = fore_elevation + myinterpolateddeltaelev;

            }


        }

    }

    return;

}



list<result_record> process_megaduna_data(Point Megaduna_GeomPts[], int megaduna_numgeompts, Point Megaduna_ElevPts[], int megaduna_numelevpts, double myNullDataZ, double filtering_distance, double buffer_distance)
{

    list<Point_along_Segment> PointsAlongMegaduna;

    Point Megaduna_FirstPoint = Megaduna_GeomPts[0];
    //printf("Point 1:  %d, %f, %f, %f\n", Megaduna_FirstPoint.id, Megaduna_FirstPoint.x, Megaduna_FirstPoint.y, Megaduna_FirstPoint.z);

    Point_along_Segment ptalongsegment0(0, Megaduna_FirstPoint, 0);
    // storing con sorting del vertice iniziale v1
    PointsAlongMegaduna.push_back(ptalongsegment0);

    double cumulated_length = 0.0;

    for (int i = 1; i < megaduna_numgeompts; i++)
    {

        // definizione e iniziali inserimenti nel set che conserva i risultati puntuali delle elevazioni lungo il segmento considerato

        set<Point_along_Segment> PtsAlongSegment; // set di punti lungo la traccia, con distanza dall'inizio del segmento

        Point Megaduna_SecondPoint = Megaduna_GeomPts[i];
        // creazione vettore v1
        Vector v1(Megaduna_FirstPoint, Megaduna_SecondPoint);
        // calcolo magnitudine v1
        double magnitude_horiz_v1 = v1.magnitude_horiz();


        // storing con sorting del vertice finale p2
        Point_along_Segment ptalongsegment1(cumulated_length+magnitude_horiz_v1, Megaduna_SecondPoint, 0);
        PtsAlongSegment.insert(ptalongsegment1);



        for (int j = 0; j < megaduna_numelevpts; j++)
        {

            Point myElevationPoint = Megaduna_ElevPts[j];

            // creazione vettore v2
            Vector v2(Megaduna_FirstPoint, myElevationPoint);

            // calcolo magnitudine v2
            double magnitude_horiz_v2 = v2.magnitude_horiz();

            // calcolo coseno angolo sotteso da v1 e v2  = (v1 x v2 )/(|v1| |v2|)
            double angle_sotteso_rad = v1.vectorsangle_horiz(v2);
            double angle_sotteso_degr = angle_sotteso_rad*180.0/M_PI;

            if (angle_sotteso_degr > 90.0) continue;

            // calcolo proiezione di v2 su v1 = v2 cos(alpha)
            double lungh_proiezione_v2_su_v1 = magnitude_horiz_v2*cos(angle_sotteso_rad);

            // calcolo della distanza del punto di elevazioni dal segmento
            double distance_elevpt_from_segment = magnitude_horiz_v2*sin(angle_sotteso_rad);

            // casi di esclusione del punto di elevazione considerato
            // se lunghezza proiezione > lunghezza v1, elevazione NON incorporata
            if (lungh_proiezione_v2_su_v1 > magnitude_horiz_v1) continue;  // il punto NON si proietta sul segmento considerato
            if (distance_elevpt_from_segment > buffer_distance) continue;  // il punto è ad una distanza superiore alla buffer_distance dal segmento considerato


            // determinazione x_pr, y_pr da proporzionalità
            double coeff_propor = lungh_proiezione_v2_su_v1/magnitude_horiz_v1;

            double delta_x = (Megaduna_SecondPoint.x - Megaduna_FirstPoint.x)*coeff_propor;
            double delta_y = (Megaduna_SecondPoint.y - Megaduna_FirstPoint.y)*coeff_propor;

            // creazione di punto di output
            Point Projected_Elevation_Point(Megaduna_FirstPoint.id, Megaduna_FirstPoint.x+delta_x, Megaduna_FirstPoint.y+delta_y, myElevationPoint.z);

            // creazione e storing con sorting del punto di elevazione proiettato, assieme alla relativa distanza, in base alla distanza da v1
            Point_along_Segment ptalongsegment(cumulated_length+lungh_proiezione_v2_su_v1, Projected_Elevation_Point, distance_elevpt_from_segment);
            PtsAlongSegment.insert(ptalongsegment);

        }


        // storing di p2 in p1, per successivo ciclo
        Megaduna_FirstPoint = Megaduna_SecondPoint;
        // updating cumulated lenght for subsequent cycle
        cumulated_length = cumulated_length + magnitude_horiz_v1;


        //  processamento e trasferimento dei dati in un vettore/lista di PtsAlongSegment
        set<Point_along_Segment>:: iterator ptsalsegm_pos;
        for (ptsalsegm_pos = PtsAlongSegment.begin(); ptsalsegm_pos != PtsAlongSegment.end(); ptsalsegm_pos++)
        {
            PointsAlongMegaduna.push_back(*ptsalsegm_pos);
        }



    }

    // interpolazione ed
    // output dei punti lungo la megaduna


    int numtotpts_in_megaduna = PointsAlongMegaduna.size();
    double elev_linearinterp[numtotpts_in_megaduna], elev_proxinterp[numtotpts_in_megaduna], proxinter_distelpt[numtotpts_in_megaduna];

    interpolate_megaduna_elevations(PointsAlongMegaduna, elev_linearinterp, elev_proxinterp, proxinter_distelpt, numtotpts_in_megaduna, myNullDataZ, filtering_distance);

    list<result_record> Megaduna_Result;

    list<Point_along_Segment>:: iterator pos;

    int n=0;
    for (pos = PointsAlongMegaduna.begin(); pos != PointsAlongMegaduna.end(); pos++)
    {
        result_record myelevationresult;

        myelevationresult.id_track = (*pos).pt.id;
        myelevationresult.s = (*pos).s;
        myelevationresult.h = (*pos).h;
        myelevationresult.x = (*pos).pt.x;
        myelevationresult.y = (*pos).pt.y;
        myelevationresult.z_orig = (*pos).pt.z;
        myelevationresult.z_linint = elev_linearinterp[n];
        myelevationresult.z_proxint = elev_proxinterp[n];
        myelevationresult.dist_proxelpt = proxinter_distelpt[n];

        Megaduna_Result.push_back(myelevationresult);

        n++;

    }



    return Megaduna_Result;


}


int main ()
{

    int i,j;

    double myNullDataAbsZ = 9999.9;
    int myMinMaxCaseFactor;


    /* DEFINIZIONE DELLA DISTANZA DI buffer */
    cout << "Enter buffer distance from track (in meters)\n";
  	double buffer_distance;
    cin >> buffer_distance;

    /* DEFINIZIONE DELLA DISTANZA DI FILTRO */
    cout << "Enter filtering distance (in meters)\n";
  	double filtering_distance;
    cin >> filtering_distance;

    /* DEFINIZIONE DEL CASO DI ANALISI - TRACCIATO DEI MINIMI O DEI MASSIMI */
    cout << "Enter case: max or min\n";
  	string analysis_case;
    cin >> analysis_case;

    if (analysis_case == "max")
    {
        myMinMaxCaseFactor = 1;
    }
    else if (analysis_case == "min")
    {
        myMinMaxCaseFactor = -1;
    }
    else
    {
        return 0;
    };


    double myNullDataZ = myMinMaxCaseFactor*myNullDataAbsZ;


    /* DEFINIZIONE DEL FILE DI INPUT DEI TRACCIATI */
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



    /* DEFINIZIONE DEL FILE DI INPUT DEI PUNTI DI ELEVAZIONE */
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


    // DEFINIZIONE DEL FILE DI OUTPUT PUNTUALE
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
    // INIZIO PROCESSAMENTO INPUT DATI
    //


    // INPUT DATI TRACCE DELLE MEGADUNE

    set<int> myMegadune_IdsUnivoci;
    multiset<int> myMegadune_IdsConDupl;
    string rec_line;

    // legge l'header del file
    getline(infile_tracciati, rec_line);

    list<Point> Megadune_Tracks;

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
        myMegadune_IdsUnivoci.insert(md_id);
        myMegadune_IdsConDupl.insert(md_id); // inserisce id della megaduna in lista con id univoci

        Point gpt(md_id, md_x, md_y, myNullDataZ);

        Megadune_Tracks.push_back(gpt);


    }


    // chiude file di input tracciati megadune
    infile_tracciati.close();



   // INPUT ELEVATION POINTS

    list<Point>::iterator pos; // iteratore della lista

    // legge l'header del file
    getline(infile_pt_elevazione, rec_line);

    int InputElevPts_num = 0;

    list<Point> InputElevationPts;

    int ep_id; long double ep_x, ep_y, ep_z;

    // lettura dei dati di input dei punti di elevazione
    while (!infile_pt_elevazione.eof())
    {

        // read record
        getline(infile_pt_elevazione, rec_line);

        if (rec_line.size() == 0) continue;

        InputElevPts_num++;
        istringstream instr(rec_line);

        instr >> ep_id >> comma >> ep_x >> comma >> ep_y >> comma >> ep_z;

        Point ept(ep_id, ep_x, ep_y, ep_z);

        InputElevationPts.push_back(ept);

    }

    // chiude file di input elevazioni punti
    infile_pt_elevazione.close();


    // OUTPUT SU SCHERMO DI DATI RIEPILOGATIVI
    cout << "num recs tracce: " << num_recs_tracc << "; num recs elevpts: " << InputElevPts_num << "\n";


    // ---------------------------------

    // PRE-PROCESSAMENTO DATI


    // DETERMINA PER OGNI MEGADUNA IL NUMERO DI PUNTI GEOMETRICI ED IL NUMERO DI ELEVATION POINTS ASSOCIATI

    int num_trace_recs_exam, num_elev_pts_exam;

    map <int, int> MegaduneSet, Megadune_numelevpts;

    set<int>::iterator p, q;

    for (p = myMegadune_IdsUnivoci.begin(); p != myMegadune_IdsUnivoci.end(); p++)
    {


        int Megaduna_id = *p;
        int Megaduna_numgeompts = 0;

        for (q = myMegadune_IdsConDupl.begin(); q != myMegadune_IdsConDupl.end(); q++)
        {
            if (*q == Megaduna_id) Megaduna_numgeompts++;
        }

        MegaduneSet[Megaduna_id] = Megaduna_numgeompts;



        int currmegaduna_numelevpts = 0;

        for (pos=InputElevationPts.begin(); pos!=InputElevationPts.end(); pos++)
        {

            if ((*pos).id == Megaduna_id) currmegaduna_numelevpts++;


        }


        Megadune_numelevpts[Megaduna_id] = currmegaduna_numelevpts;


    }


    // ---------------------------------
    // PROCESSAMENTO DATI


    map<int,int>::iterator m;


    outpointfile << "id_rec, id_track, x, y, s, h, z_orig, z_linint, z_proxint, dist_proxelpt\n";


    int id_record_result = 0;



    for (m = MegaduneSet.begin(); m != MegaduneSet.end(); m++)
    {


        // identificativo e numero vertici di megaduna analizzata
        int Megaduna_id = (*m).first;   // analysed megadune id
        int Megaduna_numgeompts = (*m).second;



        // definizione vertici singola traccia di megaduna analizzata
        Point Megaduna_GeomPts[Megaduna_numgeompts];    // array of Points that will store the megadune trace vertices

        int id_mdtr_subset = 0;

        for (pos=Megadune_Tracks.begin(); pos!=Megadune_Tracks.end(); pos++)
        {

            if ((*pos).id == Megaduna_id)
            {
                Megaduna_GeomPts[id_mdtr_subset].id = (*pos).id;
                Megaduna_GeomPts[id_mdtr_subset].x = (*pos).x;
                Megaduna_GeomPts[id_mdtr_subset].y = (*pos).y;
                Megaduna_GeomPts[id_mdtr_subset].z = (*pos).z;
                id_mdtr_subset++;
            }

        }


        // definizione dati elevazioni lungo una singola traccia di megaduna
        int Megaduna_numelevpts = Megadune_numelevpts[Megaduna_id];

        Point Megaduna_ElevPts[Megaduna_numelevpts];

        int id_elevpts_subset = 0;

        for (pos=InputElevationPts.begin(); pos!=InputElevationPts.end(); pos++)
        {

            if ((*pos).id == Megaduna_id)
            {
                Megaduna_ElevPts[id_elevpts_subset].id = (*pos).id;
                Megaduna_ElevPts[id_elevpts_subset].x = (*pos).x;
                Megaduna_ElevPts[id_elevpts_subset].y = (*pos).y;
                Megaduna_ElevPts[id_elevpts_subset].z = (*pos).z;
                id_elevpts_subset++;
            }

        }

        // analisi elevation points along a megadune trace
        list<result_record> Megaduna_Result = process_megaduna_data(Megaduna_GeomPts, Megaduna_numgeompts, Megaduna_ElevPts, Megaduna_numelevpts, myNullDataZ, filtering_distance, buffer_distance);


        // output dei risultati
        list<result_record>::iterator pos_res;

        for (pos_res=Megaduna_Result.begin(); pos_res!=Megaduna_Result.end(); pos_res++)
        {

            id_record_result++;

            ostringstream outstr;

            outstr << fixed << setprecision(3);
            outstr  << setw(10) << id_record_result << ","
                    << setw(10) << (*pos_res).id_track << ","
                    << setw(12) << (*pos_res).x << ","
                    << setw(12) << (*pos_res).y << ","
                    << setw(10) << (*pos_res).s << ","
                    << setw(10) << (*pos_res).h << ","
                    << setw(10) << (*pos_res).z_orig << ","
                    << setw(12) << (*pos_res).z_linint << ","
                    << setw(10) << (*pos_res).z_proxint << ","
                    << setw(10) << (*pos_res).dist_proxelpt;

            outpointfile << outstr.str() << "\n";


        }




    }

    outpointfile.close();

    return 0;

}




